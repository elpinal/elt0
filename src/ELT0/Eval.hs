{-# LANGUAGE BinaryLiterals #-}

module ELT0.Eval
  ( run
  , code
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
-- * The next {n:int | 0 <= n && n <= 2 } bits specify operand-format.

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

getByte :: Evaluator Word8
getByte = lift $ gets $ getCur . fst

getMask :: Word8 -> Evaluator Word8
getMask m = (.&. m) <$> getByte

getShift :: Int -> Evaluator Word8
getShift i = (`shift` i) <$> getByte

next :: Evaluator ()
next = lift $ modify $ first incOffset

getByteNext :: Evaluator Word8
getByteNext = getByte <* next

getMaskNext :: Word8 -> Evaluator Word8
getMaskNext m = getMask m <* next

getShiftNext :: Int -> Evaluator Word8
getShiftNext i = getShift i <* next

-- @i@ starts from 0.
test :: Int -> Evaluator Bool
test i = flip testBit i <$> getByte

testNext :: Int -> Evaluator Bool
testNext i = test i <* next

run :: Code -> File
run c = snd . snd $ evalc eval c

evalc :: Evaluator a -> Code -> (Maybe a, (Code, File))
evalc e c = runEval e (c, Map.empty)

runEval :: Evaluator a -> (Code, File) -> (Maybe a, (Code, File))
runEval e s = flip runState s $ runMaybeT e

eval :: Evaluator ()
eval = forever eval1

eval1 :: Evaluator ()
eval1 = getMask 0b11111 >>= f
  where
    f :: Word8 -> Evaluator ()
    f 0 = mov
    f 1 = add
    f 2 = sub
    f 10 = halt
    -- 3 = and
    -- 4 = or
    -- 5 = not
    -- 6 = shl
    -- 7 = shr
    -- 8 = ifJmp
    -- 9 = jmp

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
-- Given 'False', 'readOperand' reads a byte and fetching a word from a corresponding register.
-- Given 'True', 'readOperand' reads a word.
-- In Both cases, the offset is automatically incremented.
readOperand :: Bool -> Evaluator Word32
readOperand False = readReg
readOperand True  = readWord

-- | 5 bits (0) | 1 bit (0) | 2 bits (ignored) | 8 bits | 8 bits
-- | 5 bits (0) | 1 bit (1) | 2 bits (ignored) | 8 bits | 32 bits
mov :: Evaluator ()
mov = do
  sp <- testNext 5 -- an operand-format specifier
  r <- getByteNext
  v <- readOperand sp
  modifyReg r v

-- | 5 bits (1) | 2 bit (0) | 1 bits (ignored) | 8 bits | 8 bits | 8 bits
-- | 5 bits (1) | 2 bit (1) | 1 bits (ignored) | 8 bits | 32 bits | 8 bits
-- | 5 bits (1) | 2 bit (2) | 1 bits (ignored) | 8 bits | 8 bits | 32 bits
-- | 5 bits (1) | 2 bit (3) | 1 bits (ignored) | 8 bits | 32 bits | 32 bits
add :: Evaluator ()
add = do
  sp1 <- test 5
  sp2 <- testNext 6
  r <- getByteNext
  v1 <- readOperand sp1
  v2 <- readOperand sp2
  modifyReg r $ v1 + v2 -- Overflow may occur.

-- | 5 bits (2) | 2 bit (0) | 1 bits (ignored) | 8 bits | 8 bits | 8 bits
-- | 5 bits (2) | 2 bit (1) | 1 bits (ignored) | 8 bits | 32 bits | 8 bits
-- | 5 bits (2) | 2 bit (2) | 1 bits (ignored) | 8 bits | 8 bits | 32 bits
-- | 5 bits (2) | 2 bit (3) | 1 bits (ignored) | 8 bits | 32 bits | 32 bits
sub :: Evaluator ()
sub = do
  sp1 <- test 5
  sp2 <- testNext 6
  r <- getByteNext
  v1 <- readOperand sp1
  v2 <- readOperand sp2
  modifyReg r $ v1 - v2 -- Overflow may occur.

-- | 5 bits (10 in dec) | 3 bits (ignored)
halt :: Evaluator ()
halt = empty
