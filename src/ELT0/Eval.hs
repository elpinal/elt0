{-# LANGUAGE BinaryLiterals #-}

module ELT0.Eval
  ( run
  , runFile
  , runStack
  , runMachine
  , code
  , Code
  , Offset
  , File
  , Machine(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor
import Data.Bits
import Data.Array.Unboxed
import Data.List
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
-- Each instruction amounts to several bytes.
--
-- * The first 5 bits represent an opcode.
--   More precicely, they determine which kind of instruction is selected.
--   The correspondence between decimal numbers and mnemonics is defined as follows:
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
--   * 11 -> salloc
--   * 12 -> sfree
--   * 13 -> sld
-- * The next { n:int | 0 <= n && n <= 2 } bits specify a format of operands.
--   { n } depends on the opcode of an instruction in question.
-- * The rest of an instruction represents its operands.

-- | 'File' represents a register file.
type File = Map.Map Word8 Word32

type Offset = Word32

type Code = (UArray Word32 Word8, Offset)

type Stack = [Word32]

data Machine = Machine Code File Stack
  deriving (Eq, Show)

getCode :: Machine -> Code
getCode (Machine c _ _) = c

getFile :: Machine -> File
getFile (Machine _ f _) = f

getStack :: Machine -> Stack
getStack (Machine _ _ s) = s

mapCode :: (Code -> Code) -> Machine -> Machine
mapCode f (Machine c rf s) = Machine (f c) rf s

mapFile :: (File -> File) -> Machine -> Machine
mapFile f (Machine c rf s) = Machine c (f rf) s

mapStack :: (Stack -> Stack) -> Machine -> Machine
mapStack f (Machine c rf s) = Machine c rf $ f s

accessStack :: Word32 -> Stack -> Word32
accessStack w s = s `genericIndex` w

type Evaluator = MaybeT (State Machine)

code :: [Word8] -> Code
code l = (listArray (1, fromInteger . toInteger $ length l) l, 1)

getCur :: Code -> Word8
getCur (a, o) = a ! o

setOffset :: Offset -> Code -> Code
setOffset o (a, _) = (a, o)

incOffset :: Code -> Code
incOffset = second (+ 1)

jumpTo :: Offset -> Evaluator ()
jumpTo o = lift $ modify $ mapCode $ setOffset o

readByte :: Evaluator Word8
readByte = lift $ gets $ getCur . getCode

readMask :: Word8 -> Evaluator Word8
readMask m = (.&. m) <$> readByte

inc :: Evaluator ()
inc = lift $ modify $ mapCode incOffset

fetchByte :: Evaluator Word8
fetchByte = readByte <* inc

-- @i@ starts from 0.
readTest :: Int -> Evaluator Bool
readTest i = flip testBit i <$> readByte

fetchTest :: Int -> Evaluator Bool
fetchTest i = readTest i <* inc

-- | Evaluates a program, then returns a calculated register file.
run :: Code -> File
run c = runFile c Map.empty

-- |
-- Evaluates a program with an initial register file, then returns
-- a calculated register file.
runFile :: Code -> File -> File
runFile c f = getFile . runMachine $ Machine c f mempty

runStack :: Code -> Stack -> Stack
runStack c s = getStack . runMachine $ Machine c Map.empty s

runMachine :: Machine -> Machine
runMachine = snd . runEvaluator program

runEvaluator :: Evaluator a -> Machine -> (Maybe a, Machine)
runEvaluator e s = flip runState s $ runMaybeT e

program :: Evaluator ()
program = forever instruction

instruction :: Evaluator ()
instruction = readMask 0b11111 >>= f
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
    f 11 = salloc
    f 12 = sfree
    f 13 = sld

modifyReg :: Word8 -> Word32 -> Evaluator ()
modifyReg r v = lift $ modify $ mapFile $ Map.insert r v

getVal :: Word8 -> Evaluator Word32
getVal r = lift $ gets $ f r . getFile
  where
    f = Map.findWithDefault $ error $ "getVal: no such register declared: " ++ show r

-- In big-endian order.
buildWord32 :: [Word8] -> Word32
buildWord32 = foldl (\acc x -> shift acc 8 .|. toWord32 x) 0
  where
    toWord32 :: Word8 -> Word32
    toWord32 = fromInteger . toInteger

fetchReg :: Evaluator Word32
fetchReg = fetchByte >>= getVal

fetchWord :: Evaluator Word32
fetchWord = buildWord32 <$> replicateM 4 fetchByte

-- |
-- Given 'False', 'fetchOperand' reads a byte and fetches a word from a corresponding register.
-- Given 'True', 'fetchOperand' reads a word.
-- In Both cases, the offset is automatically incremented.
fetchOperand :: Bool -> Evaluator Word32
fetchOperand False = fetchReg
fetchOperand True  = fetchWord

-- Format:
-- | 5 bits (0) | 1 bit (0) | 2 bits (ignored) | 8 bits | 8 bits
-- | 5 bits (0) | 1 bit (1) | 2 bits (ignored) | 8 bits | 32 bits
mov :: Evaluator ()
mov = do
  sp <- fetchTest 5 -- an operand-format specifier
  r <- fetchByte
  v <- fetchOperand sp
  modifyReg r v

-- Register-Operand-Operand (ROO) format scheme, called ROOs:
-- For all o.
-- | 5 bits (o) | 2 bit (0) | 1 bits (ignored) | 8 bits | 8 bits | 8 bits
-- | 5 bits (o) | 2 bit (1) | 1 bits (ignored) | 8 bits | 32 bits | 8 bits
-- | 5 bits (o) | 2 bit (2) | 1 bits (ignored) | 8 bits | 8 bits | 32 bits
-- | 5 bits (o) | 2 bit (3) | 1 bits (ignored) | 8 bits | 32 bits | 32 bits
roo :: (Word32 -> Word32 -> Word32) -> Evaluator ()
roo f = do
  sp1 <- readTest 5
  sp2 <- fetchTest 6
  r <- fetchByte
  v1 <- fetchOperand sp1
  v2 <- fetchOperand sp2
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
  sp <- fetchTest 5
  r <- fetchByte
  v <- fetchOperand sp
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
  sp <- fetchTest 5
  rr <- fetchReg -- The right value of a register.
  v <- fetchOperand sp
  when (rr == 0) $
    jumpTo v

-- "Jump" instruction.
-- Format:
-- | 5 bits (9) | 1 bit (0) | 2 bits (ignored) | 8 bits
-- | 5 bits (9) | 1 bit (1) | 2 bits (ignored) | 32 bits
jmp :: Evaluator ()
jmp = do
  sp <- fetchTest 5
  v <- fetchOperand sp
  jumpTo v

-- Format:
-- | 5 bits (10 in decimal) | 3 bits (ignored)
halt :: Evaluator ()
halt = empty

pushWord :: Word32 -> Evaluator ()
pushWord w = lift . modify . mapStack $ (w :)

-- "Stack allocation" instruction.
-- Allocated slots are uninitialized. In fact, they are set to zero.
-- Format:
-- | 5 bits (11 in decimal) | 3 bits (ignored) | 32 bits
salloc :: Evaluator ()
salloc = inc >> fetchWord >>= loop
  where
    loop cnt
      | cnt == 0 = pure ()
      | otherwise = pushWord 0 *> loop (cnt - 1)

dropWord :: Evaluator ()
dropWord = lift . modify $ mapStack f
  where
    f (_ : xs) = xs

-- "Stack free" instruction.
-- Format:
-- | 5 bits (12 in decimal) | 3 bits (ignored) | 32 bits
sfree :: Evaluator ()
sfree = inc >> fetchWord >>= loop
  where
    loop cnt
      | cnt == 0 = pure ()
      | otherwise = dropWord *> loop (cnt - 1)

getFromStack :: Word32 -> Evaluator Word32
getFromStack w = lift . gets $ accessStack w . getStack

-- "Stack load" instruction.
-- Load a word from the nth slot of the stack into a register.
-- The index starts from 0; the 0th slot is the top of the stack.
-- Format:
-- | 5 bits (13 in decimal) | 3 bits (ignored) | 8 bits | 32 bits
sld :: Evaluator ()
sld = join $ inc >> modifyReg <$> fetchByte <*> (fetchWord >>= getFromStack)
