module ELT0.Parser
  ( mainParser
  , run
  , insts
  , inst
  , operand
  , reg
  ) where

import Text.Parsec
import Text.Parsec.Char
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
  { reservedNames =
    [ "mov"
    , "add"
    , "sub"
    , "and"
    , "or"
    , "not"
    , "shl"
    , "shr"
    ]
  }

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

inst :: Parser Inst
inst = choice . map try $
  [ reserved lexer "mov" $> Mov <*> reg <*> operand
  , reserved lexer "add" *> inst3op Add
  , reserved lexer "sub" *> inst3op Sub
  , reserved lexer "and" *> inst3op And
  , reserved lexer "or"  *> inst3op Or
  , reserved lexer "not" $> Not <*> reg <*> operand
  , reserved lexer "shl" *> inst3op Shl
  , reserved lexer "shr" *> inst3op Shr
  ]

inst3op :: (Reg -> Operand -> Operand -> a) -> Parser a
inst3op op = op <$> reg <*> operand <*> operand

instSep :: Parser ()
instSep = () <$ symbol lexer "\n" <|> skipMany1 (semi lexer)

insts :: Parser [Inst]
insts = inst `sepEndBy` instSep

mainParser :: String -> Either ParseError [Inst]
mainParser = run $ insts <* eof

run :: Parser a -> String -> Either ParseError a
run p = parse p "<filename>"
