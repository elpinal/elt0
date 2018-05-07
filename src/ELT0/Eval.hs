{-# LANGUAGE BinaryLiterals #-}

module ELT0.Eval
  ( run
  , runFile
  , code
  , Code
  , Offset
  , File
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor
import Data.Bits
import Data.Array.Unboxed
import qualified Data.Map.Lazy as Map
import Data.Word

--- Convention ---
--
-- * Least significant bit first.
-- * Most significant byte first, namely big-endian.
--
--
--- The Machine Code Format ---
--
-- 1 instruction amount to several bytes.
--
-- * The first 5 bits are for opcodes.
--   * 0 -> mov
--   * 1 -> add
--   * 2 -> sub
--   * 3 -> and
--   * 4 -> or
--   * 5 -> not
--   * 6 -> shl
--   * 7 -> shr
--   * 8 -> if-jmp
--   * 9 -> jmp
--   * 10 -> halt
-- * The next { n:int | 0 <= n && n <= 2 } bits specify operand-format.

-- | 'File' represents a register file.
type File = Map.Map Word8 Word32

type Offset = Word32

type Code = (UArray Word32 Word8, Offset)

type Evaluator = MaybeT (State (Code, File))

code :: [Word8] -> Code
code l = (listArray (1, fromInteger . toInteger $ length l) l, 1)

getCur :: Code -> Word8
getCur (a, o) = a ! o

setOffset :: Offset -> Code -> Code
setOffset o (a, _) = (a, o)

incOffset :: Code -> Code
incOffset = second (+ 1)

jumpTo :: Offset -> Evaluator ()
jumpTo o = lift $ modify $ first $ setOffset o

getByte :: Evaluator Word8
getByte = lift $ gets $ getCur . fst

getMask :: Word8 -> Evaluator Word8
getMask m = (.&. m) <$> getByte

next :: Evaluator ()
next = lift $ modify $ first incOffset

getByteNext :: Evaluator Word8
getByteNext = getByte <* next

-- @i@ starts from 0.
test :: Int -> Evaluator Bool
test i = flip testBit i <$> getByte

testNext :: Int -> Evaluator Bool
testNext i = test i <* next

-- | Evaluates a program, then returns a calculated register file.
run :: Code -> File
run c = runFile c Map.empty

-- |
-- Evaluates a program with an initial register file, then returns
-- a calculated register file.
runFile :: Code -> File -> File
runFile c = snd . snd . curry (runEvaluator program) c

runEvaluator :: Evaluator a -> (Code, File) -> (Maybe a, (Code, File))
runEvaluator e s = flip runState s $ runMaybeT e

program :: Evaluator ()
program = forever instruction

instruction :: Evaluator ()
instruction = getMask 0b11111 >>= f
  where
    f :: Word8 -> Evaluator ()
    f 0 = mov
    f 1 = add
    f 2 = sub
    f 3 = band
    f 4 = bor
    f 5 = bnot
    f 6 = shl
    f 7 = shr
    f 8 = ifJmp
    f 9 = jmp
    f 10 = halt

modifyReg :: Word8 -> Word32 -> Evaluator ()
modifyReg r v = lift $ modify $ second $ Map.insert r v

getVal :: Word8 -> Evaluator Word32
getVal r = lift $ gets $ f r . snd
  where
    f = Map.findWithDefault $ error $ "getVal: no such register declared: " ++ show r

-- In big-endian order.
buildWord32 :: [Word8] -> Word32
buildWord32 = foldl (\acc x -> shift acc 8 .|. toWord32 x) 0
  where
    toWord32 :: Word8 -> Word32
    toWord32 = fromInteger . toInteger

readReg :: Evaluator Word32
readReg = getByteNext >>= getVal

readWord :: Evaluator Word32
readWord = buildWord32 <$> replicateM 4 getByteNext

-- |
-- Given 'False', 'readOperand' reads a byte and fetches a word from a corresponding register.
-- Given 'True', 'readOperand' reads a word.
-- In Both cases, the offset is automatically incremented.
readOperand :: Bool -> Evaluator Word32
readOperand False = readReg
readOperand True  = readWord

-- Format:
-- | 5 bits (0) | 1 bit (0) | 2 bits (ignored) | 8 bits | 8 bits
-- | 5 bits (0) | 1 bit (1) | 2 bits (ignored) | 8 bits | 32 bits
mov :: Evaluator ()
mov = do
  sp <- testNext 5 -- an operand-format specifier
  r <- getByteNext
  v <- readOperand sp
  modifyReg r v

-- Register-Operand-Operand (ROO) format scheme, called ROOs:
-- For all o.
-- | 5 bits (o) | 2 bit (0) | 1 bits (ignored) | 8 bits | 8 bits | 8 bits
-- | 5 bits (o) | 2 bit (1) | 1 bits (ignored) | 8 bits | 32 bits | 8 bits
-- | 5 bits (o) | 2 bit (2) | 1 bits (ignored) | 8 bits | 8 bits | 32 bits
-- | 5 bits (o) | 2 bit (3) | 1 bits (ignored) | 8 bits | 32 bits | 32 bits
roo :: (Word32 -> Word32 -> Word32) -> Evaluator ()
roo f = do
  sp1 <- test 5
  sp2 <- testNext 6
  r <- getByteNext
  v1 <- readOperand sp1
  v2 <- readOperand sp2
  modifyReg r $ v1 `f` v2

-- Format: ROOs(1)
add :: Evaluator ()
add = roo (+) -- Overflow may occur.

-- Format: ROOs(2)
sub :: Evaluator ()
sub = roo (-) -- Overflow may occur.

-- Format: ROOs(3)
-- Bitwise "and" instruction.
band :: Evaluator ()
band = roo (.&.)

-- Format: ROOs(4)
-- Bitwise "or" instruction.
bor :: Evaluator ()
bor = roo (.|.)

-- Bitwise "not" instruction.
-- Format:
-- | 5 bits (5) | 1 bit (0) | 2 bits (ignored) | 8 bits | 8 bits
-- | 5 bits (5) | 1 bit (1) | 2 bits (ignored) | 8 bits | 32 bits
bnot :: Evaluator ()
bnot = do
  sp <- testNext 5
  r <- getByteNext
  v <- readOperand sp
  modifyReg r $ complement v

toInt :: Word32 -> Int
toInt = fromInteger . toInteger

-- Format: ROOs(6)
-- Description: Logical left shift.
--
-- Semantics:
-- The first argument is a destination register.
-- The second argument is logically shifted left by the number of bits
-- specified by the third argument.
shl :: Evaluator ()
shl = roo f
  where
    f x y = shiftL x $ toInt y

-- Format: ROOs(7)
-- Description: Logical right shift.
--
-- Semantics:
-- The first argument is a destination register.
-- The second argument is logically shifted right by the number of bits
-- specified by the third argument.
shr :: Evaluator ()
shr = roo f
  where
    f x y = shiftR x $ toInt y

-- "Conditional Jump" instruction.
-- Jump is executed only when the value of a register given as the first
-- argument equals zero.
-- Format:
-- | 5 bits (8) | 1 bit (0) | 2 bits (ignored) | 8 bits | 8 bits
-- | 5 bits (8) | 1 bit (1) | 2 bits (ignored) | 8 bits | 32 bits
ifJmp :: Evaluator ()
ifJmp = do
  sp <- testNext 5
  rr <- readReg -- The right value of a register.
  v <- readOperand sp
  when (rr == 0) $
    jumpTo v

-- "Jump" instruction.
-- Format:
-- | 5 bits (9) | 1 bit (0) | 2 bits (ignored) | 8 bits
-- | 5 bits (9) | 1 bit (1) | 2 bits (ignored) | 32 bits
jmp :: Evaluator ()
jmp = do
  sp <- testNext 5
  v <- readOperand sp
  jumpTo v

-- Format:
-- | 5 bits (10 in decimal) | 3 bits (ignored)
halt :: Evaluator ()
halt = empty
