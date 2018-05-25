module ELT0.Parser
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
import Data.Bifunctor
import Data.Functor
import qualified Data.Map.Lazy as Map
import Data.Word

import ELT0.Parser.Lexer
import ELT0.Program

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

appMinimal :: Minimal a -> Maybe Token -> Maybe (Parse1 a)
appMinimal = fmap . runMinimal

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
  fmap f p = p { runParser = \xs -> first (fmap f) $ runParser p xs }

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

option :: Minimal a -> Parser (Maybe a)
option m = Parser $ f . decons
  where
    f (mt, rest) =
      case maybe Other1 (runMinimal m . fst) mt of
        Other1 -> (Parsed Nothing, cons mt rest)
        Parsed1 x -> (Parsed $ Just x, rest)

orEof :: Minimal a -> Parser (Maybe a)
orEof m = Parser $ first f . decons
  where
    f Nothing = Parsed Nothing
    f (Just t) =
      case runMinimal m $ fst t of
        Other1 -> Fail (Just t) $ expected m
        Parsed1 x -> Parsed $ Just x

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
    f t0 = if t0 == t then return () else Nothing

label :: Minimal String
label = minimal f ["label"]
  where
    f (Ident s) = return s
    f _ = Nothing

lBrace :: Minimal ()
lBrace = token LBrace "left brace"

rBrace :: Minimal ()
rBrace = token RBrace "right brace"

brace :: Parser a -> Parser a
brace p = do
  fromMinimal lBrace
  x <- p
  fromMinimal rBrace
  return x

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
int = token IntType $ show "int"

typeM :: Minimal (Parser Type)
typeM = int $> return Int -|- lBrace $> (Code <$> parseFile)

typ :: Parser Type
typ = do
  a <- fromMinimal typeM
  a

parseFile :: Parser Env
parseFile = do
  e <- row1 >>= p
  fromMinimal rBrace
  return e
  where
    rows = do
      m <- option comma
      case m of
        Just () -> (:) <$> row <*> rows
        Nothing -> return []

    p Nothing = return env
    p (Just x) = do
      xs <- rows
      return $ env { file = Map.fromList $ x : xs }

row1 :: Parser (Maybe (Reg, Type))
row1 = do
  mr <- option register
  case mr of
    Nothing -> return Nothing
    Just r -> do
      fromMinimal colon
      t <- typ
      return $ Just (r, t)

row :: Parser (Reg, Type)
row = do
  r <- fromMinimal register
  fromMinimal colon
  t <- typ
  return (r, t)

inst :: Parser (Maybe Inst)
inst = do
  ma <- option $ foldl1 (-|-)
    [ f TMov "mov" $> (Mov <$> fromMinimal register <*> fromMinimal operand)
    , f TAdd "add" $> rnn Add
    , f TSub "sub" $> rnn Sub
    , f TAnd "and" $> rnn And
    , f TOr  "or"  $> rnn Or
    , f TNot "not" $> (Not <$> fromMinimal register <*> fromMinimal numeric)
    , f TShl "shl" $> rnn Shl
    , f TShr "shr" $> rnn Shr
    , f TIf  "if"  $> (If <$> fromMinimal register <*> fromMinimal place)
    , f TSalloc "salloc" $> (Salloc <$> fromMinimal word')
    , f TSfree  "sfree"  $> (Sfree <$> fromMinimal word')
    , f TSld "sld" $> (Sld <$> fromMinimal register <*> fromMinimal word')
    , f TSst "sst" $> (Sst <$> fromMinimal word' <*> fromMinimal operand)
    ]
  maybe (return Nothing) (fmap Just) ma
  where
    f t e = minimal (g t) [e]
    g m0 (Mnem m) = if m == m0 then return () else Nothing
    g _ _ =  Nothing

rnn :: (Reg -> Numeric -> Numeric -> Inst) -> Parser Inst
rnn f = f <$> fromMinimal register <*> fromMinimal numeric <*> fromMinimal numeric

halt :: Minimal ()
halt = token Halt "halt"

jmp :: Minimal ()
jmp = token Jmp "jmp"

terminator :: Parser (Maybe Place)
terminator = do
  a <- fromMinimal $ halt $> return Nothing -|- jmp $> (Just <$> fromMinimal place)
  a

block :: Parser (Maybe Block)
block = do
  ms <- option label
  case ms of
    Nothing -> return Nothing
    Just s -> do
      e <- fromMinimal lBrace *> parseFile
      fromMinimal colon
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
  | Trailing [TokenP]
  | Lexer LexError
  deriving (Eq, Show)

parse :: [TokenP] -> Either Error Program
parse i = case runParser program i of
  (Parsed p, xs) -> case xs of
    [] -> return p
    _ -> Left $ Trailing xs
  (Fail m e, _) -> Left $ Unexpected m e

-- | Parses a program from 'String'.
fromString :: String -> Either Error Program
fromString s = first Lexer (runLexer lexer s) >>= parse
