module ELT0.Parser
  ( mainParser
  , runParser
  , runLexer
  , inst
  , jmp
  , label
  , reg
  , numeric
  , lexer
  , lex1
  , Token(..)
  , Mnemonic(..)
  , fromToken
  , Position
  , newPosition
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Char
import Data.Functor.Identity
import Data.Word

import ELT0.Program

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
  deriving (Eq, Show)

type TokenP = (Token, Position)

place :: Position -> Token -> TokenP
place p t = (t, p)

fromToken :: TokenP -> Token
fromToken = fst

data Token
  = Ident String
  | Mnem Mnemonic
  | Jmp
  | Digits Word32 -- not followed by alphabets.
  -- | Zero -- not followed by alphanum.
  | Newline
  | RegToken Word8
  | Colon
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
  | Numeric -- operands except labels, namely values and registers
  | OperandL -- operands (including labels)
  | NewlineLit
  | LabelLit
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
      p' ts = (fmap $ first f) <$> p ts

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
    f p '\n'               = return $ Just (Newline, p)
    f p ':'                = return $ Just (Colon, p)
    f _ '%'                = lift (while (/= '\n')) >> lex1
    f p x                  = throwE $ IllegalChar x p

-- Precondition: @isDigit x@ must hold.
lexWord :: Position -> Char -> Lexer (Maybe TokenP)
lexWord p x = do
  s <- lift $ while isDigit
  b <- lift notFollowedByLetter
  unless b $
    lift getPos >>= throwE . FollowedByAlpha (x : s)
  fmap (Just . place p . Digits) $ lexDigits p (x : s) >>= validImm32 p

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
      f w = return . place p $ RegToken . fromInteger $ toInteger w
lexLetters p a @ (x : _) | isAsciiUpper x = throwE $ UpperNonReg a p
lexLetters p a = return (f a, p)
  where
    f :: String -> Token
    f "mov" = Mnem TMov
    f "add" = Mnem TAdd
    f "sub" = Mnem TSub
    f "and" = Mnem TAnd
    f "or"  = Mnem TOr
    f "not" = Mnem TNot
    f "shl" = Mnem TShl
    f "shr" = Mnem TShr
    f "if"  = Mnem TIf
    f "jmp" = Jmp -- Notice that this is not Mnem.
    f a = Ident a

digitToWord :: Num a => Char -> a
digitToWord = fromInteger . toInteger . digitToInt

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiUpper c || isAsciiLower c

isAlphanum :: Char -> Bool
isAlphanum c = isAsciiAlpha c || isDigit c

lexer :: Lexer [TokenP]
lexer = lex1 >>= maybe (return []) (\t -> (t :) <$> lexer)

mainParser :: String -> Either Error Program
mainParser s = runIdentity . runExceptT $ mainParser' s

mainParser' :: Monad m => String -> ExceptT Error m Program
mainParser' s = do
  ts <- withExceptT LexError $ ExceptT . return $ runLexer lexer s
  p <- ExceptT . return $ ParseError +++ maybe (Program []) fst $ runParser parser $ ts
  return $ p

exactSkip :: Token -> Parser ()
exactSkip x = Parser f
  where
    f (t : ts) | fromToken t == x = Right $ Just ((), ts)
    f (t : ts) = Left . ExpectToken x $ Just t
    f [] = Left $ ExpectToken x Nothing

option :: Token -> Parser ()
option x = Parser f
  where
    f (t : ts) | fromToken t == x = Right $ Just ((), ts)
    f (t : ts) = Right Nothing
    f [] = Right Nothing

predExact :: (Token -> Maybe a) -> TokenKind -> Parser a
predExact p k = Parser f
  where
    f (t : ts) = case p $ fromToken t of
      (Just x) -> Right $ Just (x, ts)
      Nothing -> Left $ Expect k $ Just t
    f [] = Left $ Expect k Nothing

predEOF :: (TokenP -> Either ParseError a) -> Parser a
predEOF p = Parser f
  where
    f (t : ts) = Just . (\a -> (a, ts)) <$> p t
    f [] = Right Nothing

predOption :: (Token -> Maybe a) -> Parser a
predOption p = Parser f
  where
    f (t : ts) = Right $ (\a -> (a, ts)) <$> p (fromToken t)
    f [] = Right Nothing

skipMany :: Alternative f => f a -> f ()
skipMany = void . many

skipSome :: Alternative f => f a -> f ()
skipSome = void . some

parser :: Parser Program
parser = space *> p <* space
  where
    space = skipMany $ option Newline
    p = Program <$> ((:) <$> block <*> many (break *> block))
    break = skipSome $ option Newline

block :: Parser Block
block = Block <$> label <*> many (break *> inst) <*> (break *> jmp)
  where
    break = skipSome $ option Newline

label :: Parser String
label = predEOF p <* exactSkip Colon
  where
    p (Ident s, _) = Right s
    p t = Left $ Expect LabelLit $ Just t

jmp :: Parser Operand
jmp = predExact op Mnemonic *> operandL
  where
    op Jmp = Just ()
    op _ = Nothing

inst :: Parser Inst
inst = join $ predOption p
  where
    p :: Token -> Maybe (Parser Inst)
    p (Mnem m) = Just $ f m
    p _ = Nothing

    f :: Mnemonic -> Parser Inst
    f TMov = inst2opL Mov
    f TAdd = inst3op Add
    f TSub = inst3op Sub
    f TAnd = inst3op And
    f TOr  = inst3op Or
    f TNot = inst2op Not
    f TShl = inst3op Shl
    f TShr = inst3op Shr
    f TIf  = ifJmp

inst2op :: (Reg -> Numeric -> a) -> Parser a
inst2op f = f <$> reg <*> numeric

inst2opL :: (Reg -> Operand -> a) -> Parser a
inst2opL f = f <$> reg <*> operandL

inst3op :: (Reg -> Numeric -> Numeric -> a) -> Parser a
inst3op f = f <$> reg <*> numeric <*> numeric

ifJmp :: Parser Inst
ifJmp = If <$> (reg <* exactSkip Jmp) <*> operandL

reg :: Parser Reg
reg = predEOF f
  where
    f (RegToken w, _) = return $ Reg w
    f t = Left $ Expect RegisterLit $ return t

numeric :: Parser Numeric
numeric = predExact f Numeric
  where
    f (Digits w) = Just $ wordN w
    f (RegToken w) = Just $ registerN w -- TODO: duplicate of `reg`.
    f t = Nothing

operandL :: Parser Operand
operandL = predExact f OperandL
  where
    f (Digits w) = Just $ wordO w
    f (RegToken w) = Just $ Register $ Reg w -- TODO: duplicate of `reg`.
    f (Ident s) = Just $ labelO s
    f t = Nothing

runLexer :: Lexer a -> String -> Either LexError a
runLexer l s = runStream s position $ runExceptT l
