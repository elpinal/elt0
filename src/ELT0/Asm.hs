module ELT0.Asm
  ( assemble
  , B.hPut
  , resolve
  ) where

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Semigroup
import Data.Word

import ELT0.Program

assemble :: Program -> B.ByteString
assemble = B.pack . resolve . assemble'

assemble' :: Program -> Recorder
assemble' (Program bs) = foldl accBlock mempty bs

type Address = Word32

type AddressBook = Map.Map String Address

accBlock :: Recorder -> Block -> Recorder
accBlock prev (Block l is mp) = r { book = Map.insert l (lengthR prev) $ book r }
  where
    r = prev <> block is mp

block :: [Inst] -> Maybe Place -> Recorder
block is mp = foldMap inst is <> terminator mp

reg :: Reg -> Builder
reg (Reg w) = word8 w

word :: W -> Builder
word (W w) = word32BE w

label :: String -> Recorder
label = record

val :: Val -> Recorder
val (Word w) = fromBuilder $ word w
val (Label s) = label s

operand :: Operand -> Recorder
operand (Value v) = val v
operand (Register r) = fromBuilder $ reg r

place :: Place -> Recorder
place (PLabel s) = label s
place (PRegister r) = fromBuilder $ reg r

numeric :: Numeric -> Builder
numeric (NWord w) = word w
numeric (NRegister r) = reg r

setIfValue :: IsValue a => a -> Int -> Word8
setIfValue x i | isValue x = bit i
setIfValue _ _ = 0

-- | 'opfs' encodes an opcode and an operand-format specifier.
opfs :: IsValue a => a -> Word8 -> Word8
opfs x b = b .|. setIfValue x 5

inst :: Inst -> Recorder
inst (Mov r o) = word8 (opfs o 0) <> reg r |- operand o
inst (If r p)  = word8 (opfs p 8) <> reg r |- place p
inst (Sst w o) = word8 (opfs o 14) <> word32BE w |- operand o
inst i = fromBuilder $ inst' i

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
inst' _ = error "unreachable"

rnn :: Word8 -> Reg -> Numeric -> Numeric -> Builder
rnn opcode r n1 n2 = word8 (opcode .|. setIfValue n1 5 .|. setIfValue n2 6) <> reg r <> numeric n1 <> numeric n2

rn :: Word8 -> Reg -> Numeric -> Builder
rn opcode r n = word8 (opfs n opcode) <> reg r <> numeric n

terminator :: Maybe Place -> Recorder
terminator = maybe halt jmp

halt :: Recorder
halt = fromBuilder $ word8 10

jmp :: Place -> Recorder
jmp (PRegister r) = fromBuilder $ word8 9 <> reg r
jmp (PLabel s)    = word8 (setBit 9 5) |- record s

record :: String -> Recorder
record s = Recorder
  { builder = replicate 4 0x00
  , holes = \a -> Map.singleton a s
  , book = mempty
  }

fromBuilder :: Builder -> Recorder
fromBuilder b = Recorder
  { builder = b
  , holes = const mempty
  , book = mempty
  }

infixr 5 |-
(|-) :: Builder -> Recorder -> Recorder
x |- y = fromBuilder x <> y

type Builder = [Word8]

data Recorder = Recorder
  { builder :: Builder
  , holes :: Address -> Map.Map Address String
  , book :: AddressBook
  }

instance Semigroup Recorder where
  x <> y = Recorder
    { builder = builder x <> builder y
    , holes = \a -> holes x a <> holes y (a + genericLength (builder x))
    , book = book x <> book y
    }

instance Monoid Recorder where
  mempty = Recorder
    { builder = mempty
    , holes = const mempty
    , book = mempty
    }
  mappend = (<>)

word8 :: Word8 -> Builder
word8 w = [w]

word32BE :: Word32 -> Builder
word32BE w = word32BE' w []

word32BE' :: Word32 -> Builder -> Builder
word32BE' _ xs | length xs == 4 = xs
word32BE' w xs                  = word32BE' (shiftR w 8) $ fromIntegral w .&. 0xff : xs

lengthR :: Num i => Recorder -> i
lengthR = genericLength . builder

resolve :: Recorder -> Builder
resolve r = foldl f (builder r) $ Map.assocs $ holes r 0
  where
    f :: Builder -> (Address, String) -> Builder
    f = uncurry . update (book r)

update :: AddressBook -> Builder -> Address -> String -> Builder
update b xs a s = let (x, y) = genericSplitAt a xs in x ++ word32BE with ++ drop 4 y
  where
    with = Map.findWithDefault (error $ "no such label: " ++ show s) s b + 1
