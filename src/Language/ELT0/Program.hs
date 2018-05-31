module Language.ELT0.Program
  ( Program(..)
  , Block(..)
  , Type(..)
  , Env(..)
  , env
  , File
  , Stack
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

data Inst
  = Mov Reg Operand
  | Add Reg Numeric Numeric
  | Sub Reg Numeric Numeric
  | And Reg Numeric Numeric
  | Or  Reg Numeric Numeric
  | Not Reg Numeric
  | Shl Reg Numeric Numeric
  | Shr Reg Numeric Numeric
  | If  Reg Place
  | Salloc Word32
  | Sfree  Word32
  | Sld    Reg Word32
  | Sst    Word32 Operand -- Allow labels to be used with the "sst" instruction.
  deriving (Eq, Show)

newtype Program = Program [Block]
  deriving (Eq, Show)

data Block = Block String Env [Inst] (Maybe Place)
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
  { file :: File
  , stack :: Stack
  }
  deriving (Eq, Show)

-- | An empty 'Env'.
env :: Env
env = Env
  { file = mempty
  , stack = mempty
  }

type File = Map.Map Reg Type

type Stack = [Maybe Type]

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

instance Display Env where
  displayS e = showString "Code" . f (file e) . s (stack e)
    where
      f i = if i == mempty then id else displayBrace $ j $ map pair $ Map.assocs i
      s i = if i == mempty then id else displayBrack $ j $ map slot i

      j = foldr (.) id . intersperse (showString ", ")
      pair (r, t) = displayS r . showString ": " . displayS t
      -- "ns" stands for nonsense; see [Stack-Based Typed Assembly Language] (1998) by Morrisett et al.
      slot Nothing = showString "ns"
      slot (Just t) = displayS t

instance Display Block where
  displayS (Block l e is m) = showString l . showChar ' ' . displayS e . showString ":\n" .  x . end m
    where
      x = foldr (\i s -> displayS i . showChar '\n' . s) id is
      end (Just p) = showString "jmp " . displayS p
      end Nothing = showString "halt"

instance Display Program where
  displayS (Program bs) = foldr (\b s -> displayS b . showString "\n\n" . s) id bs

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
