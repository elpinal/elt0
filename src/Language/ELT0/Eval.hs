{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.ELT0.Eval
  ( run
  , runFile
  , runS
  , runFS
  , code
  , Code
  , Offset
  , File
  , Machine(..)
  ) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.Bits
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
--   * 14 -> sst
-- * The next { n:int | 0 <= n && n <= 2 } bits specify a format of operands.
--   { n } depends on the opcode of an instruction in question.
-- * The rest of an instruction represents its operands.

-- | 'File' represents a register file.
type File = Map.Map Word8 Word32

type Offset = Word32

type Code = (UArray Word32 Word8, Offset)

data Vector s = Vector
  { content :: STUArray s Integer Word32
  , cap :: Integer
  , len :: Integer
  }
  deriving Eq

newtype Stack a = Stack { runStack :: a }
  deriving (Eq, Functor)

data Machine s = Machine
  { text :: Code
  , file :: File
  , stack :: Stack (Vector s)
  }
  deriving Eq

vector :: ST s (Vector s)
vector = do
  a <- newArray_ (1, 0)
  return Vector
    { content = a
    , cap = 0
    , len = 0
    }

copy :: STUArray s Integer Word32 -> STUArray s Integer Word32 -> Integer -> Integer -> ST s ()
copy new old i n
  | i >= n    = return ()
  | otherwise = readArray old (i + 1) >>= writeArray new (i + 1) >> copy new old (i + 1) n

putStack :: Monad m => Stack (Vector s) -> StateT (Machine s) m ()
putStack s = modify $ \m -> m { stack = s }

mapStack :: (Stack (Vector s) -> Stack (Vector s)) -> Machine s -> Machine s
mapStack f m = m { stack = f $ stack m }

mapCode :: (Code -> Code) -> Machine s -> Machine s
mapCode f m = m { text = f $ text m }

mapFile :: (File -> File) -> Machine s -> Machine s
mapFile f m = m { file = f $ file m }

type Evaluator s = MaybeT (StateT (Machine s) (ST s))

code :: [Word8] -> Code
code l = (listArray (1, fromIntegral $ length l) l, 1)

getCur :: Code -> Word8
getCur (a, o) = a ! o

setOffset :: Offset -> Code -> Code
setOffset o (a, _) = (a, o)

incOffset :: Code -> Code
incOffset = second (+ 1)

jumpTo :: Offset -> Evaluator s ()
jumpTo o = lift $ modify $ mapCode $ setOffset o

readByte :: Evaluator s Word8
readByte = lift $ gets $ getCur . text

readMask :: Word8 -> Evaluator s Word8
readMask m = (.&. m) <$> readByte

inc :: Evaluator s ()
inc = lift $ modify $ mapCode incOffset

fetchByte :: Evaluator s Word8
fetchByte = readByte <* inc

-- @i@ starts from 0.
readTest :: Int -> Evaluator s Bool
readTest i = flip testBit i <$> readByte

fetchTest :: Int -> Evaluator s Bool
fetchTest i = readTest i <* inc

fetchShiftR :: Int -> Evaluator s Word8
fetchShiftR i = flip shiftR i <$> fetchByte

-- | Evaluates a program, then returns a calculated register file.
run :: Code -> File
run c = runFile c Map.empty

-- |
-- Evaluates a program with an initial register file, then returns
-- a calculated register file.
runFile :: Code -> File -> File
runFile c f = runST $ do
  v <- vector
  fmap file . runMachine $ Machine
    { text = c
    , file = f
    , stack = Stack v
    }

-- |
-- Evaluates a program with an initial stack, then returns a calculated stack.
runS :: Code -> [Word32] -> [Word32]
runS c xs = runST $ do
  let l = genericLength xs
  inv <- thaw (listArray (1, l) xs :: UArray Integer Word32)
  v <- runStack . stack <$> runMachine Machine
    { text = c
    , file = mempty
    , stack = Stack Vector { content = inv, cap = l, len = l }
    }
  a <- unsafeFreeze $ content v
  return $ genericTake (len v) $ elems (a :: UArray Integer Word32)

-- |
-- Evaluates a program with an initial register file and stack, then
-- returns a calculated register file and stack.
runFS :: Code -> File -> [Word32] -> (File, [Word32])
runFS c f xs = runST $ do
  let l = genericLength xs
  inv <- thaw (listArray (1, l) xs :: UArray Integer Word32)
  m <- runMachine Machine
    { text = c
    , file = f
    , stack = Stack Vector { content = inv, cap = l, len = l }
    }
  a <- unsafeFreeze $ content $ runStack $ stack m
  return (file m, genericTake (len $ runStack $ stack m) $ elems (a :: UArray Integer Word32))

runMachine :: Machine s -> ST s (Machine s)
runMachine = fmap snd . runEvaluator program

runEvaluator :: Evaluator s a -> Machine s -> ST s (Maybe a, Machine s)
runEvaluator e s = flip runStateT s $ runMaybeT e

program :: Evaluator s ()
program = forever instruction

instruction :: Evaluator s ()
instruction = readMask 0b11111 >>= f
  where
    f :: Word8 -> Evaluator s ()
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
    f 14 = sst
    f n = error $ "unknown opcode: " ++ show n

modifyReg :: Word8 -> Word32 -> Evaluator s ()
modifyReg r v = lift $ modify $ mapFile $ Map.insert r v

modifyStack :: Word8 -> Word32 -> Evaluator s ()
modifyStack i w = do
  v <- lift $ gets $ runStack . stack
  liftST $ writeArray (content v) (len v - fromIntegral i) w

getVal :: Word8 -> Evaluator s Word32
getVal r = lift $ gets $ f r . file
  where
    f = Map.findWithDefault $ error $ "getVal: no such register declared: " ++ show r

-- In big-endian order.
buildWord32 :: [Word8] -> Word32
buildWord32 = foldl (\acc x -> shift acc 8 .|. toWord32 x) 0
  where
    toWord32 :: Word8 -> Word32
    toWord32 = fromIntegral

fetchReg :: Evaluator s Word32
fetchReg = fetchByte >>= getVal

fetchWord :: Evaluator s Word32
fetchWord = buildWord32 <$> replicateM 4 fetchByte

-- |
-- Given 'False', 'fetchOperand' reads a byte and fetches a word from a corresponding register.
-- Given 'True', 'fetchOperand' reads a word.
-- In Both cases, the offset is automatically incremented.
fetchOperand :: Bool -> Evaluator s Word32
fetchOperand False = fetchReg
fetchOperand True  = fetchWord

-- Register-Operand (RO) format scheme, called ROs:
-- For all o.
-- | 5 bits (o) | 1 bit (0) | 2 bits (ignored) | 8 bits | 8 bits
-- | 5 bits (o) | 1 bit (1) | 2 bits (ignored) | 8 bits | 32 bits
ro :: (Word32 -> Word32) -> Evaluator s ()
ro f = do
  sp <- fetchTest 5 -- an operand-format specifier
  r <- fetchByte
  v <- fetchOperand sp
  modifyReg r $ f v

-- Format: ROs(0)
mov :: Evaluator s ()
mov = ro id

-- Register-Operand-Operand (ROO) format scheme, called ROOs:
-- For all o.
-- | 5 bits (o) | 2 bit (0) | 1 bits (ignored) | 8 bits | 8 bits | 8 bits
-- | 5 bits (o) | 2 bit (1) | 1 bits (ignored) | 8 bits | 32 bits | 8 bits
-- | 5 bits (o) | 2 bit (2) | 1 bits (ignored) | 8 bits | 8 bits | 32 bits
-- | 5 bits (o) | 2 bit (3) | 1 bits (ignored) | 8 bits | 32 bits | 32 bits
roo :: (Word32 -> Word32 -> Word32) -> Evaluator s ()
roo f = do
  sp1 <- readTest 5
  sp2 <- fetchTest 6
  r <- fetchByte
  v1 <- fetchOperand sp1
  v2 <- fetchOperand sp2
  modifyReg r $ v1 `f` v2

-- Format: ROOs(1)
add :: Evaluator s ()
add = roo (+) -- Overflow may occur.

-- Format: ROOs(2)
sub :: Evaluator s ()
sub = roo (-) -- Overflow may occur.

-- Format: ROOs(3)
-- Bitwise "and" instruction.
band :: Evaluator s ()
band = roo (.&.)

-- Format: ROOs(4)
-- Bitwise "or" instruction.
bor :: Evaluator s ()
bor = roo (.|.)

-- Bitwise "not" instruction.
-- Format: ROs(5)
bnot :: Evaluator s ()
bnot = ro complement

toInt :: Word32 -> Int
toInt = fromIntegral

-- Format: ROOs(6)
-- Description: Logical left shift.
--
-- Semantics:
-- The first argument is a destination register.
-- The second argument is logically shifted left by the number of bits
-- specified by the third argument.
shl :: Evaluator s ()
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
shr :: Evaluator s ()
shr = roo f
  where
    f x y = shiftR x $ toInt y

-- "Conditional Jump" instruction.
-- Jump is executed only when the value of a register given as the first
-- argument equals zero.
-- Format:
-- | 5 bits (8) | 1 bit (0) | 2 bits (ignored) | 8 bits | 8 bits
-- | 5 bits (8) | 1 bit (1) | 2 bits (ignored) | 8 bits | 32 bits
ifJmp :: Evaluator s ()
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
jmp :: Evaluator s ()
jmp = do
  sp <- fetchTest 5
  v <- fetchOperand sp
  jumpTo v

-- Format:
-- | 5 bits (10 in decimal) | 3 bits (ignored)
halt :: Evaluator s ()
halt = empty

liftST :: ST s a -> Evaluator s a
liftST = lift . lift

-- zero :: { n :: Word8 | 0 <= n && n <= 7 } -> { r :: Integer | if n == 0 then r == 8 else r == n }
zero :: Word8 -> Integer
zero 0 = 2 ^ (3 :: Int)
zero n = fromIntegral n

extendCap :: Integer -> Integer
extendCap = (* 2) . (+ 1)

allocStack :: Integer -> Evaluator s ()
allocStack n = lift $ do
  v <- gets $ runStack . stack
  let need = len v + n
  if need <= cap v
    then putStack $ Stack v { len = need }
    else do
      let newCap = max need $ extendCap $ cap v
      a <- lift $ newArray_ (1, newCap)
      lift $ copy a (content v) 0 $ len v
      putStack $ Stack Vector { content = a, cap = newCap, len = need }

-- "Stack allocation" instruction.
-- Allocated slots are initialized to an undefined value.
-- If the operand is equal to 0, it is interpreted as 8, which is equals to 2^3.
-- Format:
-- | 5 bits (11 in decimal) | 3 bits
salloc :: Evaluator s ()
salloc = fetchShiftR 5 >>= allocStack . zero

markUnused :: Integer -> Evaluator s ()
markUnused n = lift $ modify $ mapStack $ fmap f
  where
    f v = v { len = len v - n }

-- "Stack free" instruction.
-- If the operand is equal to 0, it is interpreted as 8, which is equals to 2^3.
-- Format:
-- | 5 bits (12 in decimal) | 3 bits
sfree :: Evaluator s ()
sfree = fetchShiftR 5 >>= markUnused . zero

readStack :: Word8 -> Evaluator s Word32
readStack w = lift $ do
  v <- gets $ runStack . stack
  lift $ content v `readArray` (len v - fromIntegral w)

-- "Stack load" instruction.
-- Load a word from the nth slot of the stack into a register.
-- The index starts from 0; the 0th slot is the top of the stack.
-- Format:
-- | 5 bits (13 in decimal) | 3 bits (ignored) | 8 bits | 8 bits
sld :: Evaluator s ()
sld = join $ inc >> modifyReg <$> fetchByte <*> (fetchByte >>= readStack)

-- "Stack store" instruction.
-- Store a word into the nth slot of the stack.
-- Format:
-- | 5 bits (14 in decimal) | 1 bit (0) | 2 bits (ignored) | 8 bits | 8 bits
-- | 5 bits (14 in decimal) | 1 bit (1) | 2 bits (ignored) | 8 bits | 32 bits
sst :: Evaluator s ()
sst = do
  sp <- fetchTest 5
  join $ modifyStack <$> fetchByte <*> fetchOperand sp
