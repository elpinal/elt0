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
testNext :: Int -> Evaluator Bool
testNext i = flip testBit i <$> getByteNext

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
    f 10 = halt
    -- 1 = add
    -- 2 = sub
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
getVal r = lift $ gets $ (Map.! r) . snd

-- In big-endian order.
buildWord32 :: [Word8] -> Word32
buildWord32 = foldl (\acc x -> shift acc 8 .|. toWord32 x) 0
  where
    toWord32 :: Word8 -> Word32
    toWord32 = fromInteger . toInteger

-- | 5 bits (0) | 1 bit (0) | 2 bits (ignored) | 8 bits | 8 bits
-- | 5 bits (0) | 1 bit (1) | 2 bits (ignored) | 8 bits | 32 bits
mov :: Evaluator ()
mov = do
  sp <- testNext 5 -- an operand-format specifier
  r <- getByteNext
  v <-
    case sp of
      False -> getByteNext >>= getVal
      True  -> buildWord32 <$> replicateM 4 getByteNext
  modifyReg r v

-- | 5 bits (10 in dec) | 3 bits (ignored)
halt :: Evaluator ()
halt = empty
