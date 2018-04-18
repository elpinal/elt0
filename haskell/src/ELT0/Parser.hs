module ELT0.Parser
  ( mainParser
  , run
  , insts
  , inst
  , operand
  , reg
  , commentSep
  ) where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

import Data.Char
import Data.Functor
import Data.Functor.Identity
import Data.Word

import ELT0.Program

def :: LanguageDef st
def = emptyDef

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser def

digitInteger :: Parser Integer
digitInteger = toInteger . digitToInt <$> digit

skipSpaces :: Parser ()
skipSpaces = skipMany $ char ' '

dec :: Parser Integer
dec = do
  n <- digitInteger
  f n <* notFollowedBy alphaNum <* skipSpaces
  where
    f :: Integer -> Parser Integer
    f 0 = return 0
    f n = foldl (\a n -> a*10 + n) n <$> many digitInteger

word32 :: Parser Word32
word32 = fromInteger . toInteger <$> dec

val :: Parser Val
val = Word <$> word32 <?> "value"

ident :: Parser String
ident = identifier lexer

col :: Parser String
col = colon lexer

reg :: Parser Reg
reg = flip label "register" . try $ do
  xs <- ident
  case toReg xs of
    Just n -> return $ Reg n
    Nothing -> unexpected "non-register identifier"

toReg :: String -> Maybe Integer
toReg "R0" = return 0
toReg ('R' : x : xs) | isDigit x && x /= '0' && all isDigit xs = return $ read (x : xs)
toReg _ = Nothing

operand :: Parser Operand
operand = Register <$> reg <|> Value <$> val

opname :: String -> Parser ()
opname s = do
  xs <- lookAhead ident <?> "opname"
  if xs == s
    then void ident
    else unexpected . show $ xs ++ ": not instruction"

inst :: Parser Inst
inst = choice
  [ opname "mov" $> Mov <*> reg <*> operand
  , opname "add" *> inst3op Add
  , opname "sub" *> inst3op Sub
  , opname "and" *> inst3op And
  , opname "or"  *> inst3op Or
  , opname "not" $> Not <*> reg <*> operand
  , opname "shl" *> inst3op Shl
  , opname "shr" *> inst3op Shr
  ] <?> "instruction"

inst3op :: (Reg -> Operand -> Operand -> a) -> Parser a
inst3op op = op <$> reg <*> operand <*> operand

instSep :: Parser ()
instSep = (symbol lexer "\n" <|> semi lexer) $> ()

commentSep :: Parser ()
commentSep = choice
  [ symbol lexer "%" >> manyTill anyChar instSep $> ()
  , instSep
  ]

comment :: Parser ()
comment = choice
  [ symbol lexer "%" >> manyTill anyChar ((instSep >> optional comment) <|> lookAhead eof) $> ()
  , instSep >> optional comment
  ] <?> show ";"

sepBy' :: Parser a -> Parser b -> Parser [a]
sepBy' p sep = do
  { x <- p
  ; xs <- many . try $ sep >> p
  ; return (x : xs)
  } <|> return []

insts :: Parser Program
insts = do
  optional comment
  p <- Program <$> inst `sepBy'` many1 commentSep
  optional comment
  return p

mainParser :: String -> Either ParseError Program
mainParser = run $ whiteSpace lexer >> insts <* eof

run :: Parser a -> String -> Either ParseError a
run p = parse p "<filename>"
