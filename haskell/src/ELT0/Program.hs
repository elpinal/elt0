module ELT0.Program
  ( Program(..)
  , Inst(..)
  , Reg(..)
  , Operand(..)
  , Val(..)
  , Display(..)
  ) where

import Data.Word

newtype Reg = Reg Integer
  deriving (Eq, Show)

newtype Val = Word Word32
  deriving (Eq, Show)

data Operand
  = Register Reg
  | Value Val
  deriving (Eq, Show)

data Inst
  = Mov Reg Operand
  | Add Reg Operand Operand
  | Sub Reg Operand Operand
  | And Reg Operand Operand
  | Or  Reg Operand Operand
  | Not Reg Operand
  | Shl Reg Operand Operand
  | Shr Reg Operand Operand
  deriving (Eq, Show)

newtype Program = Program [Inst]
  deriving (Eq, Show)

class Display a where
  display :: a -> String

instance Display Reg where
  display (Reg n) = 'R' : show n

instance Display Val where
  display (Word w) = show w

instance Display Operand where
  display (Register r) = display r
  display (Value v) = display v

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

instance Display Program where
  display (Program is) = foldr (\i s -> display i ++ "\n" ++ s) "" is
