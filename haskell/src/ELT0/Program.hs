module ELT0.Program
  ( Inst(..)
  , Reg(..)
  , Operand(..)
  , Val(..)
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
