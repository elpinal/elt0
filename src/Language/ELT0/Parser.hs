module Language.ELT0.Parser
  (
  -- * Minimals
    Minimal(..)
  , minimal
  , Parse1(..)
  , Token(..)
  , (-|-)

  -- * Parsers
  , Parser(..)
  , Parse(..)
  , fromMinimal
  , option

  -- * Positions
  , newPosition

  -- * Actual parsers
  , fromString
  , parse
  , comma

  -- * Errors
  , Error
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor
import Data.Functor
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Monoid
import Data.Word

import Language.ELT0.Parser.Lexer
import Language.ELT0.Program

data Parse1 a
  = Parsed1 a
  | Other1
  deriving (Eq, Show)

data Minimal a = Minimal
  { runMinimal :: Token -> Parse1 a
  , expected :: [String]
  }

instance Functor Parse1 where
  fmap f (Parsed1 x) = Parsed1 $ f x
  fmap _ Other1 = Other1

instance Applicative Parse1 where
  pure = Parsed1
  (Parsed1 f) <*> (Parsed1 x) = Parsed1 $ f x
  (Parsed1 _) <*> Other1 = Other1
  Other1 <*> (Parsed1 _) = Other1
  Other1 <*> Other1 = Other1

instance Alternative Parse1 where
  empty = Other1
  (Parsed1 x) <|> _ = Parsed1 x
  Other1 <|> y = y

instance Functor Minimal where
  fmap f m = m
    { runMinimal = fmap f . runMinimal m
    }

infixl 3 -|-
(-|-) :: Minimal a -> Minimal a -> Minimal a
x -|- y = Minimal
  { runMinimal = \t -> runMinimal x t <|> runMinimal y t
  , expected = expected x ++ expected y
  }

data Parse a
  = Parsed a
  | Fail (Maybe TokenP) [String]
  deriving (Eq, Show)

data Parser a = Parser { runParser :: [TokenP] -> (Parse a, [TokenP]) }

instance Functor Parse where
  fmap f (Parsed x) = Parsed $ f x
  fmap _ (Fail x y) = Fail x y

instance Applicative Parse where
  pure = Parsed
  (Parsed f) <*> (Parsed x) = Parsed $ f x
  (Parsed _) <*> (Fail x y) = Fail x y
  (Fail x y) <*> (Parsed _) = Fail x y
  (Fail x y) <*> (Fail _ _) = Fail x y

instance Functor Parser where
  fmap f p = p { runParser = first (fmap f) . runParser p }

instance Applicative Parser where
  pure x = Parser $ \xs -> (pure x, xs)
  p1 <*> p2 = p1 { runParser = \xs ->
              let (f, ys) = runParser p1 xs in
              let (x, zs) = runParser p2 ys in
              (f <*> x, zs) }

instance Monad Parser where
  p >>= f = p { runParser = \xs ->
            let (x, ys) = runParser p xs in
            case x of
              Parsed a -> runParser (f a) ys
              Fail a b -> (Fail a b, ys) }

decons :: [a] -> (Maybe a, [a])
decons [] = (Nothing, [])
decons (x : xs) = (Just x, xs)

cons :: Maybe a -> [a] -> [a]
cons Nothing xs = xs
cons (Just x) xs = x : xs

fromMinimal :: Minimal a -> Parser a
fromMinimal m = Parser $ first f . decons
  where
    f mt =
      case maybe Other1 (runMinimal m . fst) mt of
        Other1 -> Fail mt $ expected m
        Parsed1 x -> Parsed x

withPosition :: Minimal a -> Parser (a, Position)
withPosition m = Parser $ first f . decons
  where
    f Nothing = Fail Nothing $ expected m
    f mt @ (Just (t, p)) =
      case runMinimal m t of
        Other1 -> Fail mt $ expected m
        Parsed1 x -> Parsed (x, p)

option :: Minimal a -> Parser (Maybe a)
option m = Parser $ f . decons
  where
    f (mt, rest) =
      case maybe Other1 (runMinimal m . fst) mt of
        Other1 -> (Parsed Nothing, cons mt rest)
        Parsed1 x -> (Parsed $ Just x, rest)

minimal :: (Token -> Maybe a) -> [String] -> Minimal a
minimal p e = Minimal
    { runMinimal = f
    , expected = e
    }
  where
    f = maybe Other1 Parsed1 . p

token :: Token -> String -> Minimal ()
token t s = minimal f [s]
  where
    f t0 = unless (t0 == t) Nothing

label :: Minimal String
label = minimal f ["label"]
  where
    f (Ident s) = return s
    f _ = Nothing

lBrace :: Minimal ()
lBrace = token LBrace "left brace"

rBrace :: Minimal ()
rBrace = token RBrace "right brace"

lBrack :: Minimal ()
lBrack = token LBrack "left brack"

rBrack :: Minimal ()
rBrack = token RBrack "right brack"

register :: Minimal Reg
register = minimal f ["register"]
  where
    f (RegToken w) = return $ Reg w
    f _ = Nothing

word' :: Minimal Word32
word' = minimal f ["word"]
  where
    f (Digits w) = return w
    f _ = Nothing

word :: Minimal W
word = W <$> word'

liftedWord8 :: Minimal Word8
liftedWord8 = minimal f ["lifted 8-bit integer"]
  where
    f (Digits w)
      | w == 0 = Nothing
      | w <= 8 = return $ fromIntegral w
      | 9 <= w = Nothing
    f _ = Nothing

word8 :: Minimal Word8
word8 = minimal f ["8-bit integer"]
  where
    f (Digits w)
      | w <= fromIntegral (maxBound :: Word8) = return $ fromIntegral w
      | otherwise = Nothing
    f _ = Nothing

value :: Minimal Val
value = Word <$> word -|- Label <$> label

operand :: Minimal Operand
operand = Register <$> register -|- Value <$> value

numeric :: Minimal Numeric
numeric = NRegister <$> register -|- NWord <$> word

place :: Minimal Place
place = PRegister <$> register -|- PLabel <$> label

colon :: Minimal ()
colon = token Colon "colon"

comma :: Minimal ()
comma = token Comma "comma"

int :: Minimal ()
int = token IntType $ show "Int"

code :: Minimal ()
code = token CodeType $ show "Code"

typeM :: Minimal (StateT Context Parser Type)
typeM = int $> return Int -|- code $> (Code <$> parseEnv True)

typ :: StateT Context Parser Type
typ = join $ lift $ fromMinimal typeM

parseFile :: StateT Context Parser File
parseFile = rows <* lift (fromMinimal rBrace)

rows :: StateT Context Parser File
rows = evalContT $ do
  mr <- liftParser $ option register
  r <- mr !? Map.empty
  liftParser $ fromMinimal colon
  lift typ >>= f . Map.singleton r
  where
    f :: File -> ContT File (StateT Context Parser) File
    f m = do
      x <- liftParser $ option comma
      x !? m
      (r, p, t) <- lift row
      if r `Map.member` m
        then err r (reg r, p) >>= f
        else f (Map.insert r t m)

    err :: Reg -> TokenP -> ContT File (StateT Context Parser) File
    err r t = liftParser $ Parser $ \xs -> (Fail (Just t) ["register distinct from at least " ++ display r], xs)

    reg (Reg w) = RegToken w

liftParser :: Parser a -> ContT b (StateT c Parser) a
liftParser = lift . lift

(!?) :: Monad m => Maybe a -> r -> ContT r m a
Nothing !? x = ContT $ const $ return x
Just x !? _ = ContT ($ x)

row :: StateT Context Parser (Reg, Position, Type)
row = do
  (r, p) <- lift $ withPosition register
  lift $ fromMinimal colon
  t <- typ
  return (r, p, t)

parseStack :: StateT Context Parser Stack
parseStack = (slot1 >>= p) <* lift (fromMinimal rBrack)
  where
    p Nothing = return mempty
    p (Just x) = (x :) <$> slots

    slots = do
      m <- lift $ option comma
      case m of
        Just () -> (:) <$> slot <*> slots
        Nothing -> return []

slot1 :: StateT Context Parser (Maybe (Slot Type))
slot1 = do
  ma <- slotM >>= lift . option
  case ma of
    Nothing -> return Nothing
    Just a -> Just <$> a

slot :: StateT Context Parser (Slot Type)
slot = join $ slotM >>= lift . fromMinimal

slotM :: Monad m => StateT Context m (Minimal (StateT Context Parser (Slot Type)))
slotM = do
  tv <- bTyVar
  return $ fmap Slot <$> typeM -|- ns $> return Nonsense -|- return <$> tv

ns :: Minimal ()
ns = token NS $ show "NS"

bTyVar :: Monad m => StateT Context m (Minimal (Slot a))
bTyVar = do
  ctx <- get
  return $ minimal (f ctx) ["bound type variable"]
  where
    f ctx (Ident s) = StackVar s <$> elemIndex s ctx
    f _ _ = Nothing

typeAssignment :: Parser [String]
typeAssignment = option tyVar >>= maybe (return []) ((<$> typeAssignment) . (:))

tyVar :: Minimal String
tyVar = minimal f ["type variable"]
  where
    f (Ident s) = return s
    f _ = Nothing

(^>) :: Monad m => m (Maybe a) -> m b -> m (Maybe b)
x ^> y = x >>= maybe (return Nothing) (const $ Just <$> y)

parseEnv :: Bool -> StateT Context Parser Env
parseEnv escape = do
  xs <- lift typeAssignment
  modify $ f xs
  mf <- lift (option lBrace) ^> parseFile
  ms <- lift (option lBrack) ^> parseStack
  when escape $
    modify $ drop $ length xs
  return Env
    { binding = xs
    , file = fromMaybe mempty mf
    , stack = fromMaybe mempty ms
    }
  where
    f xs ys = foldl (flip (:)) ys xs

stArgs :: StateT Context Parser [Stack]
stArgs = do
  m <- lift $ option lBrack
  case m of
    Nothing -> return []
    Just () -> do
      ms <- lift (option lBrack) ^> parseStack
      maybe end f ms
  where
    f :: Stack -> StateT Context Parser [Stack]
    f s = do
      ms <- lift (option lBrack) ^> parseStack
      (s :) <$> maybe end f ms
    end = lift (fromMinimal rBrack) >> return []

stApp :: Minimal a -> StateT Context Parser (STApp a)
stApp m = do
  x <- lift $ fromMinimal m
  ss <- stArgs
  return $ STApp x ss

inst :: StateT Context Parser (Maybe Inst)
inst = do
  ma <- lift $ option $ foldl1 (-|-)
    [ f TMov "mov" $> (Mov <$> fm register <*> stApp operand)
    , f TAdd "add" $> rnnl Add
    , f TSub "sub" $> rnnl Sub
    , f TAnd "and" $> rnnl And
    , f TOr  "or"  $> rnnl Or
    , f TNot "not" $> (Not <$> fm register <*> fm numeric)
    , f TShl "shl" $> rnnl Shl
    , f TShr "shr" $> rnnl Shr
    , f TIf  "if"  $> (If <$> fm register <*> stApp place)
    , f TSalloc "salloc" $> (Salloc <$> fm liftedWord8)
    , f TSfree  "sfree"  $> (Sfree <$> fm liftedWord8)
    , f TSld "sld" $> (Sld <$> fm register <*> fm word8)
    , f TSst "sst" $> (Sst <$> fm word8 <*> stApp operand)
    ]
  maybe (return Nothing) (fmap Just) ma
  where
    f t e = minimal (g t) [e]
    g m0 (Mnem m) = unless (m == m0) Nothing
    g _ _ =  Nothing
    fm = lift . fromMinimal
    rnnl = lift . rnn

rnn :: (Reg -> Numeric -> Numeric -> Inst) -> Parser Inst
rnn f = f <$> fromMinimal register <*> fromMinimal numeric <*> fromMinimal numeric

halt :: Minimal ()
halt = token Halt "halt"

jmp :: Minimal ()
jmp = token Jmp "jmp"

terminator :: StateT Context Parser (Maybe (STApp Place))
terminator = join $ lift . fromMinimal $ halt $> return Nothing -|- jmp $> (Just <$> stApp place)

block :: Parser (Maybe Block)
block = do
  ms <- option label
  case ms of
    Nothing -> return Nothing
    Just s -> flip evalStateT [] $ do
      lift $ fromMinimal code
      e <- parseEnv False
      lift $ fromMinimal colon
      is <- p
      mp <- terminator
      return $ Just $ Block s e is mp
  where
    p = inst >>= maybe (return []) (\i -> (i :) <$> p)

program :: Parser Program
program = Program <$> p
  where
    p = block >>= maybe (return []) (\b -> (b :) <$> p)

data Error
  = Unexpected (Maybe TokenP) [String]
  | Trailing TokenP
  | Lexer LexError
  deriving (Eq, Show)

instance Display Error where
  displayS (Unexpected mt es) = showString "expected one of [" . e . showString "], but " . g
    where
      e = appEndo . foldMap Endo . intersperse (showString ", ") . map showString $ es
      g = maybe (showString "reached the end of input") (\t -> showString "got " . shows t) mt
  displayS (Trailing t) = shows t
  displayS (Lexer e) = shows e

parse :: [TokenP] -> Either Error Program
parse i = case runParser program i of
  (Parsed p, xs) -> case xs of
    [] -> return p
    (t : _) -> Left $ Trailing t
  (Fail m e, _) -> Left $ Unexpected m e

-- | Parses a program from 'String'.
fromString :: String -> Either Error Program
fromString s = first Lexer (runLexer lexer s) >>= parse

type Context = [String]
