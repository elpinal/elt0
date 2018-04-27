module ELT0.Program
  ( Program(..)
  , Block(..)
  , Inst(..)
  , Reg(..)
  , Operand(..)
  , Numeric(..)
  , Place(..)
  , Val(..)
  , W(..)
  , Display(..)
  , wordO
  , labelO
  , wordN
  , registerN
  , labelP
  , registerP
  ) where

import Data.Word

newtype Reg = Reg Word8
  deriving (Eq, Show)

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
  deriving (Eq, Show)

newtype Program = Program [Block]
  deriving (Eq, Show)

data Block = Block String [Inst] Place
  deriving (Eq, Show)

class Display a where
  display :: a -> String

instance Display Reg where
  display (Reg n) = 'R' : show n

instance Display W where
  display (W w) = show w

instance Display Operand where
  display (Register r) = display r
  display (Value v) = display v

instance Display Numeric where
  display (NRegister r) = display r
  display (NWord w) = display w

instance Display Val where
  display (Word w) = display w
  display (Label s) = s

instance Display Place where
  display (PRegister r) = display r
  display (PLabel s) = s

instance Display Inst where
  display = foldl1 (\x y -> x ++ " " ++ y) . display'

display' :: Inst -> [String]
display' (Mov r o)     = ["mov", display r, display o]
display' (Add r o1 o2) = ["add", display r, display o1, display o2]
display' (Sub r o1 o2) = ["sub", display r, display o1, display o2]
display' (And r o1 o2) = ["and", display r, display o1, display o2]
display' (Or  r o1 o2) = ["or" , display r, display o1, display o2]
display' (Not r o)     = ["not", display r, display o]
display' (Shl r o1 o2) = ["shl", display r, display o1, display o2]
display' (Shr r o1 o2) = ["shr", display r, display o1, display o2]
display' (If r p)      = ["if" , display r, "jmp", display p]

instance Display Block where
  display (Block l is p) = l ++ ":\n" ++
                           foldr (\i s -> display i ++ "\n" ++ s) "" is
                           ++ "jmp " ++ display p

instance Display Program where
  display (Program bs) = foldr (\b s -> display b ++ "\n\n" ++ s) "" bs

wordO :: Word32 -> Operand
wordO = Value . Word . W

labelO :: String -> Operand
labelO = Value . Label

wordN :: Word32 -> Numeric
wordN = NWord . W

registerN :: Word8 -> Numeric
registerN = NRegister . Reg

labelP :: String -> Place
labelP = PLabel

registerP :: Word8 -> Place
registerP = PRegister . Reg
