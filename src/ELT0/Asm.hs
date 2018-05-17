module ELT0.Asm
  ( assemble
  , block
  ) where

import Data.Bits
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Lazy as Map
import Data.Monoid
import Data.Word

import ELT0.Program

assemble :: Program -> Builder
assemble (Program bs) = let (r, ab) = foldl (accBlock ab) (mempty, mempty) bs in r

type Address = Word32

type AddressBook = Map.Map String Address

accBlock :: AddressBook -> (Builder, AddressBook) -> Block -> (Builder, AddressBook)
accBlock ab (prevB, prevAb) (Block l is mp) = (prevB <> block is mp ab, newAb)
  where
    -- FIXME:
    -- It may be slow due to the conversion 'toLazyByteString'.
    -- Moreover, the coercion from Int64 to Word32 can lead to unexpected behavior.
    addr :: Word32
    addr = fromIntegral . B.length $ toLazyByteString prevB

    newAb :: AddressBook
    newAb = if l `Map.member` prevAb
      then error $ "duplicate labels: " ++ show l
      else Map.insert l addr prevAb

block :: [Inst] -> Maybe Place -> AddressBook -> Builder
block is mp = foldMap inst is <> terminator mp

reg :: Reg -> Builder
reg (Reg w) = word8 w

word :: W -> Builder
word (W w) = word32BE w

label :: String -> AddressBook -> Builder
label s = word32BE . resolve s

val :: Val -> AddressBook -> Builder
val (Word w) = const $ word w
val (Label s) = label s

operand :: Operand -> AddressBook -> Builder
operand (Value v) = val v
operand (Register r) = const $ reg r

place :: Place -> AddressBook -> Builder
place (PLabel s) = label s
place (PRegister r) = const $ reg r

numeric :: Numeric -> Builder
numeric (NWord w) = word w
numeric (NRegister r) = reg r

setIfValue :: IsValue a => a -> Int -> Word8
setIfValue x i | isValue x = bit i
setIfValue _ _ = 0

-- | 'opfs' encodes a opcode and an operand-format specifier.
opfs :: IsValue a => a -> Word8 -> Word8
opfs x b = b .|. setIfValue x 5

inst :: Inst -> AddressBook -> Builder
inst (Mov r o) ab = word8 (opfs o 0) <> reg r <> operand o ab
inst (If r p)  ab = word8 (opfs p 8) <> reg r <> place p ab
inst (Sst w o) ab = word8 (opfs o 14) <> word32BE w <> operand o ab
inst i _ = inst' i

inst' :: Inst -> Builder
inst' (Add r n1 n2) = rnn 1 r n1 n2
inst' (Sub r n1 n2) = rnn 2 r n1 n2
inst' (And r n1 n2) = rnn 3 r n1 n2
inst' (Or  r n1 n2) = rnn 4 r n1 n2
inst' (Not r n)     = rn 5 r n
inst' (Shl r n1 n2) = rnn 6 r n1 n2
inst' (Shr r n1 n2) = rnn 7 r n1 n2
inst' (Salloc w)    = word8 11 <> word32BE w
inst' (Sfree  w)    = word8 12 <> word32BE w
inst' (Sld  r w)    = word8 13 <> reg r <> word32BE w

rnn :: Word8 -> Reg -> Numeric -> Numeric -> Builder
rnn opcode r n1 n2 = word8 (opcode .|. setIfValue n1 5 .|. setIfValue n2 6) <> reg r <> numeric n1 <> numeric n2

rn :: Word8 -> Reg -> Numeric -> Builder
rn opcode r n = word8 (opfs n opcode) <> reg r <> numeric n

terminator :: Maybe Place -> AddressBook -> Builder
terminator Nothing _              = word8 10
terminator (Just (PRegister r)) _ = word8 9 <> reg r
terminator (Just (PLabel s)) ab   = word8 (setBit 9 5) <> word32BE (resolve s ab)

resolve :: String -> AddressBook -> Address
resolve s = Map.findWithDefault e s
  where
    e = error $ "could not resolve a label (" ++ show s ++ ")"
