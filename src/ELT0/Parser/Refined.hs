module ELT0.Parser.Refined
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
  ) where

import Control.Applicative
import Data.Bifunctor
import Data.Functor
import qualified Data.Map.Lazy as Map
import Data.Word

import ELT0.Parser.Lexer
import qualified ELT0.Program as Prog

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

register :: Minimal Prog.Reg
register = minimal f ["register"]
  where
    f (RegToken w) = return $ Prog.Reg w
    f _ = Nothing

word' :: Minimal Word32
word' = minimal f ["word"]
  where
    f (Digits w) = return w
    f _ = Nothing

word :: Minimal Prog.W
word = Prog.W <$> word'

value :: Minimal Prog.Val
value = Prog.Word <$> word -|- Prog.Label <$> label

operand :: Minimal Prog.Operand
operand = Prog.Register <$> register -|- Prog.Value <$> value

numeric :: Minimal Prog.Numeric
numeric = Prog.NRegister <$> register -|- Prog.NWord <$> word

place :: Minimal Prog.Place
place = Prog.PRegister <$> register -|- Prog.PLabel <$> label

colon :: Minimal ()
colon = token Colon "colon"

comma :: Minimal ()
comma = token Comma "comma"

int :: Minimal ()
int = token IntType $ show "int"

typeM :: Minimal (Parser Prog.Type)
typeM = int $> return Prog.Int -|- lBrace $> (Prog.Code <$> file)

typ :: Parser Prog.Type
typ = do
  a <- fromMinimal typeM
  a

file :: Parser Prog.Env
file = do
  e <- row1 >>= p
  fromMinimal rBrace
  return e
  where
    rows = do
      m <- option comma
      case m of
        Just () -> (:) <$> row <*> rows
        Nothing -> return []

    p Nothing = return Prog.env
    p (Just x) = do
      xs <- rows
      return $ Prog.env { Prog.file = Map.fromList $ x : xs }

row1 :: Parser (Maybe (Prog.Reg, Prog.Type))
row1 = do
  mr <- option register
  case mr of
    Nothing -> return Nothing
    Just r -> do
      fromMinimal colon
      t <- typ
      return $ Just (r, t)

row :: Parser (Prog.Reg, Prog.Type)
row = do
  r <- fromMinimal register
  fromMinimal colon
  t <- typ
  return (r, t)

inst :: Parser (Maybe Prog.Inst)
inst = do
  ma <- option $ foldl1 (-|-)
    [ f TMov "mov" $> (Prog.Mov <$> fromMinimal register <*> fromMinimal operand)
    , f TAdd "add" $> rnn Prog.Add
    , f TSub "sub" $> rnn Prog.Sub
    , f TAnd "and" $> rnn Prog.And
    , f TOr  "or"  $> rnn Prog.Or
    , f TNot "not" $> (Prog.Not <$> fromMinimal register <*> fromMinimal numeric)
    , f TShl "shl" $> rnn Prog.Shl
    , f TShr "shr" $> rnn Prog.Shr
    , f TIf  "if"  $> (Prog.If <$> fromMinimal register <*> fromMinimal place)
    , f TSalloc "salloc" $> (Prog.Salloc <$> fromMinimal word')
    , f TSfree  "sfree"  $> (Prog.Sfree <$> fromMinimal word')
    , f TSld "sld" $> (Prog.Sld <$> fromMinimal register <*> fromMinimal word')
    , f TSst "sst" $> (Prog.Sst <$> fromMinimal word' <*> fromMinimal operand)
    ]
  maybe (return Nothing) (fmap Just) ma
  where
    f t e = minimal (g t) [e]
    g m0 (Mnem m) = if m == m0 then return () else Nothing
    g _ _ =  Nothing

rnn :: (Prog.Reg -> Prog.Numeric -> Prog.Numeric -> Prog.Inst) -> Parser Prog.Inst
rnn f = f <$> fromMinimal register <*> fromMinimal numeric <*> fromMinimal numeric

halt :: Minimal ()
halt = token Halt "halt"

jmp :: Minimal ()
jmp = token Jmp "jmp"

terminator :: Parser (Maybe Prog.Place)
terminator = do
  a <- fromMinimal $ halt $> return Nothing -|- jmp $> (Just <$> fromMinimal place)
  a

block :: Parser (Maybe Prog.Block)
block = do
  ms <- option label
  case ms of
    Nothing -> return Nothing
    Just s -> do
      e <- fromMinimal lBrace *> file
      fromMinimal colon
      is <- p
      mp <- terminator
      return $ Just $ Prog.Block s e is mp
  where
    p = inst >>= maybe (return []) (\i -> (i :) <$> p)

program :: Parser Prog.Program
program = Prog.Program <$> p
  where
    p = block >>= maybe (return []) (\b -> (b :) <$> p)

data Error
  = Unexpected (Maybe TokenP) [String]
  | Trailing [TokenP]
  | Lexer LexError
  deriving (Eq, Show)

parse :: [TokenP] -> Either Error Prog.Program
parse i = case runParser program i of
  (Parsed p, xs) -> case xs of
    [] -> return p
    _ -> Left $ Trailing xs
  (Fail m e, _) -> Left $ Unexpected m e

fromString :: String -> Either Error Prog.Program
fromString s = first Lexer (runLexer lexer s) >>= parse
