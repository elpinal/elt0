{-# LANGUAGE BinaryLiterals #-}

module ELT0.Eval
  (
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor
import Data.Bits
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Lazy as Map
import Data.Word

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

type Evaluator = StateT (B.ByteString, File) Maybe

getByte :: Evaluator Word8
getByte = gets $ B.head . fst

getMask :: Word8 -> Evaluator Word8
getMask m = gets $ (.&. m) . B.head . fst

getShift :: Int -> Evaluator Word8
getShift i = gets $ (`shift` i) . B.head . fst

next :: Evaluator ()
next = modify $ first B.tail

getByteNext :: Evaluator Word8
getByteNext = getByte <* next

getMaskNext :: Word8 -> Evaluator Word8
getMaskNext m = getMask m <* next

getShiftNext :: Int -> Evaluator Word8
getShiftNext i = getShift i <* next

-- @i@ starts from 0.
testNext :: Int -> Evaluator Bool
testNext i = flip testBit i <$> getByteNext

eval :: Evaluator ()
eval = getMask 0b11111 >>= f
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
modifyReg r v = modify $ second $ Map.insert r v

getVal :: Word8 -> Evaluator Word32
getVal r = gets $ (Map.! r) . snd

-- big endian
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
halt = lift Nothing
