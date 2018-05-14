module ELT0.Asm
  ( assemble
  , block
  ) where

import Data.Bits
import Data.ByteString.Builder
import qualified Data.Map.Lazy as Map
import Data.Monoid
import Data.Word

import ELT0.Program

assemble :: Program -> Builder
assemble (Program bs) = let (r, ab) = foldl (accBlock ab) (mempty, mempty) bs in r

type Address = Word32

type AddressBook = Map.Map String Address

accBlock :: AddressBook -> (Builder, AddressBook) -> Block -> (Builder, AddressBook)
accBlock = undefined

block :: [Inst] -> Maybe Place -> AddressBook -> Builder
block is mp = foldMap inst is <> terminator mp

inst :: Inst -> AddressBook -> Builder
inst = undefined

terminator :: Maybe Place -> AddressBook -> Builder
terminator Nothing _ = word8 10
terminator (Just (PRegister r)) _ = word8 9 <> reg r
terminator (Just (PLabel s)) ab = word8 (9 .|. shiftL 1 5) <> word32BE (resolve s ab)

reg :: Reg -> Builder
reg (Reg w) = word8 w

resolve :: String -> AddressBook -> Address
resolve s ab = Map.findWithDefault e s ab
  where
    e = error $ "could not resolve a label (" ++ show s ++ ")"
