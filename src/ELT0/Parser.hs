module ELT0.Parser
  ( runLexer
  , lexer
  , lex1
  , Token(..)
  , TokenP
  , Mnemonic(..)
  , fromToken
  , Position
  , newPosition
  , LexError
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Char
import Data.Word

data Mnemonic
  = TMov
  | TAdd
  | TSub
  | TAnd
  | TOr
  | TNot
  | TShl
  | TShr
  | TIf
  | TSalloc
  | TSfree
  | TSld
  | TSst
  deriving (Eq, Show)

type TokenP = (Token, Position)

at :: Position -> Token -> TokenP
at p t = (t, p)

fromToken :: TokenP -> Token
fromToken = fst

data Token
  = Ident String
  | Mnem Mnemonic
  | Jmp
  | Halt
  | Digits Word32 -- not followed by alphabets.
  -- | Zero -- not followed by alphanum.
  | RegToken Word8
  | Colon
  | LBrace
  | RBrace
  | Comma
  | IntType
  deriving (Eq, Show)

data Error
  = LexError LexError
  | ParseError ParseError
  deriving (Eq, Show)

data LexError
  = ZeroStartDigits Position
  | IllegalChar Char Position
  | UpperNonReg String Position
  -- represents a (digits) value of String which is followed at a position by some alphabet.
  | FollowedByAlpha String Position
  | InvalidReg String Position
  | OverflowImm32 Integer Position
  deriving (Eq, Show)

data ParseError
  = Expect TokenKind (Maybe TokenP)
  | ExpectToken Token (Maybe TokenP)
  deriving (Eq, Show)

data TokenKind
  = Mnemonic
  | RegisterLit
  | Numeric -- operands except labels, namely words and registers
  | Place   -- operands except words, namely labels and registers
  | Operand
  | LabelLit
  | WordLit
  deriving (Eq, Show)

newtype Position = Position { getPosition :: (Int, Int) }
  deriving (Eq, Show)

position :: Position
position = Position (1, 1)

newPosition :: Int -> Int -> Position
newPosition l c = Position (l, c)

mapPosition :: ((Int, Int) -> (Int, Int)) -> Position -> Position
mapPosition f = Position . f . getPosition

data Stream a = Stream
  { uncons :: String -> Position -> (a, String, Position)
  }

runStream :: String -> Position -> Stream a -> a
runStream x p (Stream f) = r where (r, _, _) = f x p

getPos :: Stream Position
getPos = Stream
  { uncons = \s p -> (p, s, p)
  }

instance Functor Stream where
  fmap f (Stream g) = Stream
    { uncons = \s p -> let (x, y, z) = g s p in (f x, y, z)
    }

instance Applicative Stream where
  pure x = Stream { uncons = \s p -> (x, s, p) }
  (Stream f) <*> (Stream x) = Stream
    { uncons = \s0 p0 ->
      let (g, s1, p1) = f s0 p0 in
      let (y, s2, p2) = x s1 p1 in
        (g y, s2, p2)
    }

instance Monad Stream where
  (Stream x) >>= f = Stream
    { uncons = \s0 p0 ->
      let (y, s1, p1) = x s0 p0 in
      let (z, s2, p2) = uncons (f y) s1 p1 in
        (z, s2, p2)
    }

satisfy :: (Char -> Bool) -> Stream (Maybe Char)
satisfy f = Stream { uncons = g }
  where
    g [] p = (Nothing, [], p)
    g (x : xs) p =
      if f x
        then (return x, xs, updatePos x p)
        else (Nothing, x : xs, p)

    updatePos '\n' = mapPosition $ (+ 1) *** (const 1)
    updatePos _    = mapPosition $ second (+ 1)

char :: Stream (Maybe Char)
char = satisfy $ const True

while :: (Char -> Bool) -> Stream String
while f = satisfy f >>= maybe (return []) (\x -> (x :) <$> while f)

type Lexer = ExceptT LexError Stream

newtype Parser a = Parser { runParser :: [TokenP] -> Either ParseError (Maybe (a, [TokenP])) }

instance Functor Parser where
  fmap f (Parser p) = Parser p'
    where
      p' ts = fmap (first f) <$> p ts

instance Applicative Parser where
  pure x = Parser $ \ts -> Right $ Just (x, ts)
  liftA2 f (Parser p1) (Parser p2) = Parser p
    where
      p ts = p1 ts >>= maybe (Right Nothing) g
      g (x, ts) = p2 ts >>= maybe (Right Nothing) (h x)
      h x (y, ts) = Right $ Just (f x y, ts)

instance Monad Parser where
  (Parser p) >>= f = Parser p'
    where
      p' ts = p ts >>= maybe (Right Nothing) g
      -- g :: (a, c) -> Either e (Maybe (Parser b, c))
      g = app . first (runParser . f)

instance Alternative Parser where
  empty = Parser $ const $ Right Nothing
  (Parser p1) <|> (Parser p2) = Parser p
    where
      p ts = p1 ts >>= maybe (p2 ts) (Right . Just)

lex1 :: Lexer (Maybe TokenP)
lex1 = flip runKleisli () $ liftS getPos &&& liftS char >>> Kleisli g
  where
    liftS = Kleisli . const . lift

    g :: (Position, Maybe Char) -> Lexer (Maybe TokenP)
    g (p, m) = maybe (return Nothing) (f p) m

    f p x | isDigit x      = lexWord p x
    f p x | isAsciiAlpha x = lift (while isAlphanum) >>= fmap Just . lexLetters p . (x :)
    f _ ' '                = lex1
    f _ '\n'               = lex1
    f p ':'                = return $ Just (Colon, p)
    f p ','                = return $ Just (Comma, p)
    f p '{'                = return $ Just (LBrace, p)
    f p '}'                = return $ Just (RBrace, p)
    f _ '%'                = lift (while (/= '\n')) >> lex1
    f p x                  = throwE $ IllegalChar x p

-- Precondition: @isDigit x@ must hold.
lexWord :: Position -> Char -> Lexer (Maybe TokenP)
lexWord p x = do
  s <- lift $ while isDigit
  b <- lift notFollowedByLetter
  unless b $
    lift getPos >>= throwE . FollowedByAlpha (x : s)
  fmap (Just . at p . Digits) $ lexDigits p (x : s) >>= validImm32 p

-- Precondition: @n >= 0@ must hold.
validImm32 :: Position -> Integer -> Lexer Word32
validImm32 p n | n > toInteger (maxBound :: Word32) = throwE $ OverflowImm32 n p
validImm32 _ n = return $ fromInteger n

notFollowedByLetter :: Stream Bool
notFollowedByLetter = Stream
  { uncons = f
  }
  where
    f [] p = (True, [], p)
    f a @ (x : _) p = if isAsciiAlpha x
      then (False, a, p)
      else (True, a, p)

-- Precondition: @all isDigit ds@ must hold.
lexDigits :: Num a => Position -> String -> Lexer a
lexDigits _ "0"                 = return 0
lexDigits _ (d : ds) | d /= '0' = return $ foldl (\x y -> x*10 + digitToWord y) (digitToWord d) ds
lexDigits p _                   = throwE $ ZeroStartDigits p

lexLetters :: Position -> String -> Lexer TokenP
lexLetters p ('R' : ds) | let l = length ds, 0 < l, all isDigit ds =
  if l < 4
    then lexDigits p ds >>= f
    else throwE $ InvalidReg ds p
    where
      f :: Word16 -> Lexer TokenP
      f w | w > 255 = throwE $ InvalidReg ds p
      f w = return . at p $ RegToken . fromInteger $ toInteger w
lexLetters p "Int" = return (IntType, p)
lexLetters p a @ (x : _) | isAsciiUpper x = throwE $ UpperNonReg a p
lexLetters p a = return (f a, p)
  where
    f :: String -> Token
    f "mov"    = Mnem TMov
    f "add"    = Mnem TAdd
    f "sub"    = Mnem TSub
    f "and"    = Mnem TAnd
    f "or"     = Mnem TOr
    f "not"    = Mnem TNot
    f "shl"    = Mnem TShl
    f "shr"    = Mnem TShr
    f "if"     = Mnem TIf
    f "salloc" = Mnem TSalloc
    f "sfree"  = Mnem TSfree
    f "sld"    = Mnem TSld
    f "sst"    = Mnem TSst
    f "jmp"    = Jmp -- Notice that this is not Mnem.
    f "halt"   = Halt
    f s        = Ident s

digitToWord :: Num a => Char -> a
digitToWord = fromInteger . toInteger . digitToInt

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiUpper c || isAsciiLower c

isAlphanum :: Char -> Bool
isAlphanum c = isAsciiAlpha c || isDigit c

lexer :: Lexer [TokenP]
lexer = lex1 >>= maybe (return []) (\t -> (t :) <$> lexer)

runLexer :: Lexer a -> String -> Either LexError a
runLexer l s = runStream s position $ runExceptT l
