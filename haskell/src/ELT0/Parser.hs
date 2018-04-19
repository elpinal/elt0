module ELT0.Parser
  ( mainParser
  , runParser
  , runLexer
  , reg
  , operand
  , lexer
  , lex1
  , Token(..)
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Char
import Data.Functor.Identity
import Data.Word

import ELT0.Program

data Token
  = Ident String
  | Digits Word32 -- not followed by alphabets.
  -- | Zero -- not followed by alphanum.
  | Newline
  | RegToken Word8
  deriving (Eq, Show)

data Error
  = LexError LexError
  | ParseError ParseError
  deriving (Eq, Show)

data LexError
  = ZeroStartDigits Position
  | IllegalChar Char Position
  | UpperNonReg String Position
  | FollowedByAlpha Position
  | InvalidReg Position
  deriving (Eq, Show)

data ParseError
  = Expect TokenKind (Maybe Token)
  deriving (Eq, Show)

data TokenKind
  = Mnemonic
  | RegisterLit
  | Operand
  | NewlineLit
  deriving (Eq, Show)

newtype Position = Position { getPosition :: (Int, Int) }
  deriving (Eq, Show)

position :: Position
position = Position (1, 1)

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

    updatePos '\n' = mapPosition $ bimap (+ 1) (const 1)
    updatePos _    = mapPosition $ second (+ 1)

char :: Stream (Maybe Char)
char = satisfy $ const True

while :: (Char -> Bool) -> Stream String
while f = satisfy f >>= maybe (return []) (\x -> (x :) <$> while f)

type Lexer a = ExceptT LexError Stream a

type Parser m a = [Token] -> ExceptT ParseError m a

lex1 :: Lexer (Maybe Token)
lex1 = lift char >>= maybe (return Nothing) f
  where
    f x | isDigit x      = lexWord x
    f x | isAsciiAlpha x = lift (while isAlphanum) >>= fmap Just . lexLetters . (x :)
    f ' '                = lex1
    f '\n'               = return $ Just Newline
    f '%'                = lift (while (/= '\n')) >> lex1
    f x                  = lift getPos >>= throwE . IllegalChar x

-- Precondition: @isDigit x@ must hold.
lexWord :: Char -> Lexer (Maybe Token)
lexWord x = do
  s <- lift $ while isDigit
  b <- lift notFollowedByLetter
  unless b $
    lift getPos >>= throwE . FollowedByAlpha
  Just . Digits <$> lexDigits (x : s)

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
lexDigits :: Num a => String -> Lexer a
lexDigits "0"                 = return 0
lexDigits (d : ds) | d /= '0' = return $ foldl (\x y -> x*10 + digitToWord y) (digitToWord d) ds
lexDigits _                   = lift getPos >>= throwE . ZeroStartDigits

lexLetters :: String -> Lexer Token
lexLetters ('R' : ds) | let l = length ds, 0 < l, l < 4, all isDigit ds =
  lexDigits ds >>= f
    where
      f :: Word16 -> Lexer Token
      f w | w > 255 = lift getPos >>= throwE . InvalidReg
      f w = return $ RegToken . fromInteger $ toInteger w
lexLetters a @ (x : _) | isAsciiUpper x = lift getPos >>= throwE . UpperNonReg a
lexLetters a = return $ Ident a

digitToWord :: Num a => Char -> a
digitToWord = fromInteger . toInteger . digitToInt

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiUpper c || isAsciiLower c

isAlphanum :: Char -> Bool
isAlphanum c = isAsciiAlpha c || isDigit c

lexer :: Lexer [Token]
lexer = lex1 >>= maybe (return []) (\t -> (t :) <$> lexer)

mainParser :: String -> Either Error Program
mainParser = runIdentity . mainParser'

mainParser' :: Monad m => String -> m (Either Error Program)
mainParser' s = flip evalStateT position . runExceptT $ do
  ts <- withExceptT LexError $ ExceptT . return $ runLexer lexer s
  is <- withExceptT ParseError $ parser ts
  return $ Program is

parser :: Monad m => Parser m [Inst]
parser (Newline : ts) = parser ts
parser ts = inst ts >>= maybe (return []) f
  where
    f (i, []) = return [i]
    f (i, Newline : ts) = (i :) <$> parser ts
    f (i, t : _) = throwE $ Expect NewlineLit $ return t

inst :: Monad m => Parser m (Maybe (Inst, [Token]))
inst [] = return Nothing
inst (Ident x : ts) = return <$> f x ts
  where
    f :: Monad m => String -> Parser m (Inst, [Token])
    f "mov" = inst2op Mov
    f "add" = inst3op Add
    f "sub" = inst3op Sub
    f "and" = inst3op And
    f "or"  = inst3op Or
    f "not" = inst2op Not
    f "shl" = inst3op Shl
    f "shr" = inst3op Shr
    f x     = const $ throwE $ Expect Mnemonic $ return $ Ident x
inst (t : ts) = throwE $ Expect Mnemonic $ return t

inst2op :: Monad m => (Reg -> Operand -> a) -> Parser m (a, [Token])
inst2op f ts = do
  (r, ts) <- reg ts     >>= maybe (throwE $ Expect RegisterLit Nothing) return
  (o, ts) <- operand ts >>= maybe (throwE $ Expect Operand Nothing) return
  return (f r o, ts)

inst3op :: Monad m => (Reg -> Operand -> Operand -> a) -> Parser m (a, [Token])
inst3op f ts = do
  (r, ts)  <- reg ts     >>= maybe (throwE $ Expect RegisterLit Nothing) return
  (o1, ts) <- operand ts >>= maybe (throwE $ Expect Operand Nothing) return
  (o2, ts) <- operand ts >>= maybe (throwE $ Expect Operand Nothing) return
  return (f r o1 o2, ts)

reg :: Monad m => Parser m (Maybe (Reg, [Token]))
reg [] = return Nothing
reg (RegToken w : ts) = return $ Just (Reg w, ts)
reg (t : ts) = throwE $ Expect RegisterLit $ return t

operand :: Monad m => Parser m (Maybe (Operand, [Token]))
operand [] = return Nothing
operand (Digits w : ts) = return $ Just (Value $ Word w, ts)
operand (RegToken w : ts) = return $ Just (Register $ Reg w, ts) -- TODO: duplicate of `reg`.
operand (t : ts) = throwE $ Expect Operand $ return t

runLexer :: Lexer a -> String -> Either LexError a
runLexer l s = runStream s position $ runExceptT l

runParser :: Parser Identity a -> [Token] -> Either ParseError a
runParser p ts = runIdentity . runExceptT $ p ts
