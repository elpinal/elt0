module Language.ELT0.Program
  ( Program(..)
  , Block(..)
  , Type(..)
  , Env(..)
  , env
  , File
  , Stack
  , Slot(..)
  , STApp(..)
  , eraseST
  , Inst(..)
  , Reg(..)
  , Operand(..)
  , Numeric(..)
  , Place(..)
  , Val(..)
  , W(..)
  , IsValue(..)
  , Display(..)
  , wordO
  , labelO
  , registerO
  , wordN
  , registerN
  , labelP
  , registerP
  ) where

import Data.List
import qualified Data.Map.Lazy as Map
import Data.Monoid
import Data.Word

newtype Reg = Reg Word8
  deriving (Eq, Ord, Show)

newtype W = W Word32
  deriving (Eq, Show)

data Val
  = Word W
  | Label String
  deriving (Eq, Show)

data Operand
  = Register Reg
  | Value Val
  deriving (Eq, Show)

data Numeric
  = NRegister Reg
  | NWord W
  deriving (Eq, Show)

data Place
  = PRegister Reg
  | PLabel String
  deriving (Eq, Show)

-- | An application to stack types
data STApp a = STApp a [Stack]
  deriving (Eq, Show)

eraseST :: STApp a -> a
eraseST (STApp x _) = x

data Inst
  = Mov Reg (STApp Operand)
  | Add Reg Numeric Numeric
  | Sub Reg Numeric Numeric
  | And Reg Numeric Numeric
  | Or  Reg Numeric Numeric
  | Not Reg Numeric
  | Shl Reg Numeric Numeric
  | Shr Reg Numeric Numeric
  | If  Reg (STApp Place)
  | Salloc Word8 -- 1 to 8
  | Sfree  Word8 -- 1 to 8
  | Sld    Reg Word8
  | Sst    Word8 (STApp Operand) -- Allow labels to be used with the "sst" instruction.
  deriving (Eq, Show)

newtype Program = Program [Block]
  deriving (Eq, Show)

data Block = Block String Env [Inst] (Maybe (STApp Place))
  deriving (Eq, Show)

data Type
  -- | Word-sized unsigned integers.
  = Int
  -- |
  -- Labels which, when jumped to, expect to have values described by @Env@
  -- in the associated register.
  | Code Env
  deriving (Eq, Show)

data Env = Env
  { binding :: [String]
  , file :: File
  , stack :: Stack
  }
  deriving (Eq, Show)

-- | An empty 'Env'.
env :: Env
env = Env
  { binding = mempty
  , file = mempty
  , stack = mempty
  }

type File = Map.Map Reg Type

type Stack = [Slot Type]

data Slot a
  = Nonsense
  | Slot a
  | StackVar String Int
  deriving (Eq, Show)

class Display a where
  displayS :: a -> ShowS
  display :: a -> String
  display x = displayS x ""

instance Display Reg where
  displayS (Reg n) = showChar 'R' . shows n

instance Display W where
  displayS (W w) = shows w

instance Display Operand where
  displayS (Register r) = displayS r
  displayS (Value v) = displayS v

instance Display Numeric where
  displayS (NRegister r) = displayS r
  displayS (NWord w) = displayS w

instance Display Val where
  displayS (Word w) = displayS w
  displayS (Label s) = showString s

instance Display Place where
  displayS (PRegister r) = displayS r
  displayS (PLabel s) = showString s

instance Display a => Display (STApp a) where
  displayS (STApp x ss) = displayS x . brack (null ss) (f ss)
    where
      f = appEndo . foldMap Endo . intersperse (showChar ' ') . map displayStack
      brack True i = i
      brack False i = showChar '[' . i . showChar ']'

instance Display Inst where
  displayS (Mov r o)     = showString "mov"    |.| displayS r |.| displayS o
  displayS (Add r o1 o2) = showString "add"    |.| displayS r |.| displayS o1 |.| displayS o2
  displayS (Sub r o1 o2) = showString "sub"    |.| displayS r |.| displayS o1 |.| displayS o2
  displayS (And r o1 o2) = showString "and"    |.| displayS r |.| displayS o1 |.| displayS o2
  displayS (Or  r o1 o2) = showString "or"     |.| displayS r |.| displayS o1 |.| displayS o2
  displayS (Not r o)     = showString "not"    |.| displayS r |.| displayS o
  displayS (Shl r o1 o2) = showString "shl"    |.| displayS r |.| displayS o1 |.| displayS o2
  displayS (Shr r o1 o2) = showString "shr"    |.| displayS r |.| displayS o1 |.| displayS o2
  displayS (If r p)      = showString "if"     |.| displayS r |.| showString "jmp" |.| displayS p
  displayS (Salloc w)    = showString "salloc" |.| shows w
  displayS (Sfree w)     = showString "sfree"  |.| shows w
  displayS (Sld r w)     = showString "sld"    |.| displayS r |.| shows w
  displayS (Sst w o)     = showString "sst"    |.| shows w |.| displayS o

(|.|) :: ShowS -> ShowS -> ShowS
x |.| y = x . showChar ' ' . y

displayBrace :: ShowS -> ShowS
displayBrace s = showChar '{' . s . showChar '}'

displayBrack :: ShowS -> ShowS
displayBrack s = showChar '[' . s . showChar ']'

instance Display Type where
  displayS Int = showString "Int"
  displayS (Code e) = displayS e

instance Display a => Display (Slot a) where
  -- "NS" stands for nonsense; see [Stack-Based Typed Assembly Language] (1998) by Morrisett et al.
  displayS Nonsense = showString "NS"
  displayS (Slot x) = displayS x
  displayS (StackVar s _) = showString s

displayStack :: Stack -> ShowS
displayStack s = if s == mempty then id else displayBrack $ joinByCommas $ map displayS s

joinByCommas :: [ShowS] -> ShowS
joinByCommas = foldr (.) id . intersperse (showString ", ")

instance Display Env where
  displayS e = showString "Code" . b (binding e) . f (file e) . displayStack (stack e)
    where
      f i = if i == mempty then id else displayBrace $ joinByCommas $ map pair $ Map.assocs i

      pair (r, t) = displayS r . showString ": " . displayS t

      b :: [String] -> String -> String
      b [] = id
      b (x : xs) = showString " " . showString x . b xs

instance Display Block where
  displayS (Block l e is m) = showString l . showChar ' ' . displayS e . showString ":\n" .  x . end m
    where
      x = foldr (\i s -> displayS i . showChar '\n' . s) id is
      end (Just p) = showString "jmp " . displayS p
      end Nothing = showString "halt"

instance Display Program where
  displayS (Program bs) = foldr (.) id . intersperse (showString "\n\n") $ map displayS bs

wordO :: Word32 -> Operand
wordO = Value . Word . W

labelO :: String -> Operand
labelO = Value . Label

registerO :: Word8 -> Operand
registerO = Register . Reg

wordN :: Word32 -> Numeric
wordN = NWord . W

registerN :: Word8 -> Numeric
registerN = NRegister . Reg

labelP :: String -> Place
labelP = PLabel

registerP :: Word8 -> Place
registerP = PRegister . Reg

class IsValue a where
  isValue :: a -> Bool

instance IsValue Operand where
  isValue (Value _) = True
  isValue (Register _) = False

instance IsValue Place where
  isValue (PLabel _) = True
  isValue (PRegister _) = False

instance IsValue Numeric where
  isValue (NWord _) = True
  isValue (NRegister _) = False
