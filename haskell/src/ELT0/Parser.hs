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

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Char
import Data.Functor
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

type Lex m a = ExceptT LexError (StateT Position m) a

type Lexer m a = String -> Lex m a

type Parser m a = [Token] -> ExceptT ParseError m a

incLine :: Monad m => StateT Position m ()
incLine = modify . mapPosition $ \(x, y) -> (x + 1, 1)

incCol :: Monad m => StateT Position m ()
incCol = modify . mapPosition $ second (+ 1)

incColN :: Monad m => Int -> StateT Position m ()
incColN n | n > 0 = incCol >> incColN (n - 1)
incColN _         = return ()

incPosByChar :: Monad m => Char -> StateT Position m ()
incPosByChar '\n' = incLine
incPosByChar _    = incCol

getPos :: Monad m => StateT Position m Position
getPos = get

lex1 :: Monad m => Lexer m (Maybe (Token, String))
lex1 [] = return Nothing
lex1 (x : xs) = do
  case x of
    _ | isDigit x ->
      let (ds, ys) = first (x :) $ span isDigit xs in
        case ys of
          (y : _) | isAsciiAlpha y -> lift getPos >>= throwE . FollowedByAlpha
          _ -> fmap (first Digits) <$> lexDigits ds ys
    _ | isAsciiAlpha x ->
      let (s, ys) = first (x :) $ span isAlphanum xs in
        case s of
          ('R' : ds) | let l = length ds, 0 < l, l < 4, all isDigit ds ->
            lift (incColN $ length s) >> lexDigits ds ys >>= f
              where
                f :: Monad m => Maybe (Word16, String) -> Lex m (Maybe (Token, String))
                f (Just (w, z)) | w > 255 = lift getPos >>= throwE . InvalidReg
                f (Just (w, z)) = return $ Just (RegToken . fromInteger $ toInteger w, z)
                f Nothing = return Nothing
          (x : xs)   | isAsciiUpper x -> lift getPos >>= throwE . UpperNonReg s
          _ -> lift (incColN $ length s) $> Just (Ident s, ys)
    ' ' -> lift incCol >> lex1 xs
    '\n' -> lift incLine >> return (Just (Newline, xs))
    '%' -> let (a, b) = break (== '\n') xs in lift (incColN $ length a) >> lex1 b
    _ -> lift getPos >>= throwE . IllegalChar x

-- Precondition: @all isDigit ds@ must hold.
lexDigits :: (Monad m, Num a) => String -> String -> Lex m (Maybe (a, String))
lexDigits ds ys =
  case ds of
    "0"                 -> lift (incColN 1) $> Just (0, ys)
    (d : ds) | d /= '0' -> lift (incColN $ length ds + 1) $> Just (t, ys) where t = foldl (\x y -> x*10 + digitToWord y) (digitToWord d) ds
    _                   -> lift getPos >>= throwE . ZeroStartDigits

digitToWord :: Num a => Char -> a
digitToWord = fromInteger . toInteger . digitToInt

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiUpper c || isAsciiLower c

isAlphanum :: Char -> Bool
isAlphanum c = isAsciiAlpha c || isDigit c

lexer :: Monad m => Lexer m [Token]
lexer s = lex1 s >>= maybe (return []) (\(t, s) -> (t :) <$> lexer s)

mainParser :: String -> Either Error Program
mainParser = runIdentity . mainParser'

mainParser' :: Monad m => String -> m (Either Error Program)
mainParser' s = flip evalStateT position . runExceptT $ do
  ts <- withExceptT LexError $ lexer s
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

runLexer :: Lexer Identity a -> String -> Either LexError a
runLexer l s = runIdentity . flip evalStateT position . runExceptT $ l s

runParser :: Parser Identity a -> [Token] -> Either ParseError a
runParser p ts = runIdentity . runExceptT $ p ts
