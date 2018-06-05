{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.ELT0.Type
  ( program
  , Heap
  , fromProgram
  , Type(..)
  , env

  -- * Errors
  , TypeError(..)
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Foldable
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Word

import Language.ELT0.Program

type Heap = Map.Map String Type

fromProgram :: Program -> Either TypeError Heap
fromProgram (Program bs) = foldrM p Map.empty bs
  where
    p :: Block -> Heap -> Either TypeError Heap
    p (Block l _ _ _) h | l `Map.member` h = Left $ DuplicateLabel l
    p (Block l e _ _) h = return $ Map.insert l (Code e) h

isStackVar :: Slot a -> Bool
isStackVar (StackVar _) = True
isStackVar _ = False

lenInteger :: Stack -> Integer
lenInteger = genericLength

-- req :: w : Word8 -> s : Stack -> Either TypeError ({s0 : Stack | len s0 == w}, {s1 : Stack | len s1 == len s - w})
req :: Word8 -> Stack -> Either TypeError (Stack, Stack)
req w s | fromIntegral w <= lenInteger s = let (xs, ys) = genericSplitAt w s in
  if any isStackVar xs
    then Left $ UnderStackVar w s
    else return (xs, ys)
req w s = Left $ ShortStackZ w $ genericLength s

nth :: Word8 -> Stack -> Either TypeError Type
nth w s = req w s >>= f . snd
  where
    f [] = Left $ ShortStackZ w $ genericLength s
    f (x : _) = case x of
      Slot t -> return t
      Nonsense -> Left $ AccessToNonsense w s
      StackVar _ -> Left $ UnderStackVar w s

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x

set :: Word8 -> Type -> Stack -> Either TypeError Stack
set w t s = req w s >>= \(xs, ys) -> do
  maybe (Left $ ShortStackZ w $ genericLength s) (f xs ys) $ headMay ys
    where
      f xs ys x
        | isStackVar x = Left $ UnderStackVar w s
        | otherwise    = return $ xs ++ [Slot t] ++ tail ys

mapFile :: (File -> File) -> Env -> Env
mapFile f e = e { file = f $ file e }

mapStack :: (Stack -> Stack) -> Env -> Env
mapStack f e = e { stack = f $ stack e }

type TypeChecker = ReaderT Heap (StateT Env (Either TypeError))

data TypeError
  = MustInt Type
  | MustCode Type
  | MissingHeap String
  -- |
  -- @Mismatch e1 e2@ denotes @e1@ does not match @e2@.
  | Mismatch Env Env
  -- |
  -- @ShortStack w l@ states that it is not possible to access @w@ slots of the
  -- stack whose length is @l@.
  | ShortStack Word32 Integer
  -- |
  -- zero-indexed
  | ShortStackZ Word8 Integer
  | AccessToNonsense Word8 Stack
  | UnderStackVar Word8 Stack
  | UnboundLabel String
  | UnboundRegister Reg
  | DuplicateLabel String
  deriving (Eq, Show)

instance Display TypeError where
  displayS (MustInt t)            = showString "expected Int, but got " . displayS t
  displayS (MustCode t)           = showString "expected Code, but got " . displayS t
  displayS (MissingHeap s)        = showString "heap for " . showString s . showString " is not given" -- rare case
  displayS (Mismatch e1 e2)       = displayS e1 . showString " does not match " . displayS e2
  displayS (ShortStack w l)       = showString "stack is required to have at least " . shows w . showString " slots, but indeed its length is " . shows l
  displayS (ShortStackZ w l)      = showString "could not access to the nth (n = " . shows w . showString ") slot since its length is " . shows l
  displayS (AccessToNonsense w s) = showString "access to nonsense: " . shows w . showString " of " . shows s
  displayS (UnderStackVar w s)    = showString "access to a slot under some stack variable: " . shows w . showString " of " . shows s
  displayS (UnboundLabel s)       = showString "unbound label: " . shows s
  displayS (UnboundRegister r)    = showString "unbound register: " . displayS r
  displayS (DuplicateLabel s)     = showString "duplicate label declaration: " . shows s

getFile :: TypeChecker File
getFile = lift $ gets file

getStack :: TypeChecker Stack
getStack = lift $ gets stack

lookupHeap :: String -> TypeChecker Type
lookupHeap s = ask >>= maybe (liftEither $ Left $ UnboundLabel s) return . Map.lookup s

class Typed a b where
  typeOf :: a -> TypeChecker b

instance Typed Val Type where
  typeOf (Word _) = return Int
  typeOf (Label s) = lookupHeap s

instance Typed Reg Type where
  typeOf r = getFile >>= maybe (liftEither $ Left $ UnboundRegister r) return . Map.lookup r

instance Typed Operand Type where
  typeOf (Register r) = typeOf r
  typeOf (Value v) = typeOf v

instance Typed Numeric Type where
  typeOf (NRegister r) = typeOf r
  typeOf (NWord _) = return Int

instance Typed Place Type where
  typeOf (PRegister r) = typeOf r
  typeOf (PLabel s) = lookupHeap s

instance Typed Inst () where
  typeOf (Mov r o)     = typeOf o >>= insertFile r
  typeOf (Add r n1 n2) = intBinOp r n1 n2
  typeOf (Sub r n1 n2) = intBinOp r n1 n2
  typeOf (And r n1 n2) = intBinOp r n1 n2
  typeOf (Or  r n1 n2) = intBinOp r n1 n2
  typeOf (Not r n)     = guardInt n *> insertFile r Int
  typeOf (Shl r n1 n2) = intBinOp r n1 n2
  typeOf (Shr r n1 n2) = intBinOp r n1 n2
  typeOf (If  r p)     = void $ guardInt r >> guardMatch p
  typeOf (Salloc w)    = lift . modify $ mapStack (genericReplicate w (Nonsense) ++)
  typeOf (Sfree w)     = sfree w
  typeOf (Sld r w)     = getStack >>= liftEither . nth w >>= insertFile r
  typeOf (Sst w o)     = set w <$> typeOf o <*> getStack >>= liftEither >>= putStack

block :: Block -> ReaderT Heap (Either TypeError) ()
block (Block s _ is mp) = do
  h <- ask
  mapReaderT (g h) $ mapM_ f is >> maybe (return ()) guardMatch mp
  where
    f :: Inst -> TypeChecker ()
    f = typeOf

    g :: Heap -> StateT Env (Either TypeError) () -> Either TypeError ()
    g h m = maybe (Left $ MissingHeap s) return (Map.lookup s h) >>= fromCode >>= evalStateT m

-- | @program p h@ typechecks @p@ under the assumption @h@.
program :: Program -> Heap -> Either TypeError ()
program (Program bs) = runReaderT $ mapM_ block bs

intBinOp :: Reg -> Numeric -> Numeric -> TypeChecker ()
intBinOp r n1 n2 = guardInt n1 >> guardInt n2 >> insertFile r Int

insertFile :: Reg -> Type -> TypeChecker ()
insertFile r = lift . modify . mapFile . Map.insert r

isInt :: Type -> Bool
isInt Int = True
isInt _ = False

guardInt :: Typed a Type => a -> TypeChecker ()
guardInt n = typeOf n >>= guardE . MustInt <*> isInt

-- Note: if allowed quantification over 'Code', 'fromCode' should return also polymorphic 'Code'.
fromCode :: Type -> Either TypeError Env
fromCode (Code e) = return e
fromCode Int      = Left $ MustCode Int

liftEither :: Either TypeError a -> TypeChecker a
liftEither = lift . lift

guardMatch :: Place -> TypeChecker ()
guardMatch p = join $ match <$> lift get <*> (typeOf p >>= liftEither . fromCode)

-- |
-- @match e1 e2@ tests whether @e1@ matches @e2@.
-- The term "match" stems from "signature matching" of module systems.
-- "Signature matching" is described, for example, in
-- "Advanced Topics in Types and Programming Languages" (Pierce, editor), Chapter 8.
match :: Env -> Env -> TypeChecker ()
match e1 e2 = if e1 <: e2
  then return ()
  else liftEither $ Left $ Mismatch e1 e2

sfree :: Word8 -> TypeChecker ()
sfree w = do
  s <- lift $ gets stack
  let l = genericLength s :: Integer
  guardE (ShortStack (fromIntegral w) l) $ fromIntegral w <= l
  putStack $ genericDrop w s

putStack :: Stack -> TypeChecker ()
putStack = lift . modify . mapStack . const

guardE :: TypeError -> Bool -> TypeChecker ()
guardE e = liftEither . maybe (Left e) return . guard

class Subtyping a where
  (<:) :: a -> a -> Bool

instance Subtyping Env where
  e1 <: e2 = stack e1 == stack e2 && file e1 <: file e2

instance Subtyping File where
  -- | Width subtyping for records.
  f1 <: f2 = Map.isSubmapOf f2 f1
