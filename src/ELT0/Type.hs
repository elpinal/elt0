{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ELT0.Type
  ( program
  , env
  , Type(..)
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Word

import ELT0.Program

type Heap = Map.Map String Type

nth :: Word32 -> Stack -> Maybe Type
nth w s | w < genericLength s = s `genericIndex` w
nth _ _ | otherwise           = Nothing

set :: Word32 -> Type -> Stack -> Maybe Stack
set 0 t (_ : s) = return $ Just t : s
set w t (h : s) = (h :) <$> set (w - 1) t s
set _ _ []      = Nothing

mapFile :: (File -> File) -> Env -> Env
mapFile f e = e { file = f $ file e }

mapStack :: (Stack -> Stack) -> Env -> Env
mapStack f e = e { stack = f $ stack e }

type TypeChecker = ReaderT Heap (StateT Env Maybe)

getFile :: TypeChecker File
getFile = lift $ gets file

getStack :: TypeChecker Stack
getStack = lift $ gets stack

class Typed a b where
  typeOf :: a -> TypeChecker b

instance Typed Val Type where
  typeOf (Word _) = return Int
  typeOf (Label s) = ask >>= liftMaybe . Map.lookup s

instance Typed Reg Type where
  typeOf r = getFile >>= liftMaybe . Map.lookup r

instance Typed Operand Type where
  typeOf (Register r) = typeOf r
  typeOf (Value v) = typeOf v

instance Typed Numeric Type where
  typeOf (NRegister r) = typeOf r
  typeOf (NWord _) = return Int

instance Typed Place Type where
  typeOf (PRegister r) = typeOf r
  typeOf (PLabel s) = ask >>= liftMaybe . Map.lookup s

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
  typeOf (Salloc w)    = lift . modify $ mapStack (genericReplicate w Nothing ++)
  typeOf (Sfree w)     = sfree w
  typeOf (Sld r w)     = getStack >>= liftMaybe . nth w >>= insertFile r
  typeOf (Sst w o)     = set w <$> typeOf o <*> getStack >>= liftMaybe >>= putStack

block :: Block -> ReaderT Heap Maybe ()
block (Block s is mp) = do
  h <- ask
  mapReaderT (g h) $ mapM_ f is >> maybe (return ()) guardMatch mp
  where
    f :: Inst -> TypeChecker ()
    f = typeOf

    g :: Heap -> StateT Env Maybe () -> Maybe ()
    g h m = Map.lookup s h >>= fromCode >>= evalStateT m

-- | @program p h@ typechecks @p@ under the assumption @h@.
program :: Program -> Heap -> Maybe ()
program (Program bs) = runReaderT $ mapM_ block bs

intBinOp :: Reg -> Numeric -> Numeric -> TypeChecker ()
intBinOp r n1 n2 = guardInt n1 >> guardInt n2 >> insertFile r Int

insertFile :: Reg -> Type -> TypeChecker ()
insertFile r = lift . modify . mapFile . Map.insert r

isInt :: Type -> Bool
isInt Int = True
isInt _ = False

guardInt :: Typed a Type => a -> TypeChecker ()
guardInt n = typeOf n >>= guard . isInt

-- Note: if allowed quantification over 'Code', 'fromCode' should return also polymorphic 'Code'.
fromCode :: Type -> Maybe Env
fromCode (Code e) = return e
fromCode Int      = Nothing

liftMaybe :: Maybe a -> ReaderT Heap (StateT Env Maybe) a
liftMaybe = lift . lift

guardMatch :: Place -> TypeChecker ()
guardMatch p = match <$> (typeOf p >>= liftMaybe . fromCode) <*> lift get >>= guard

-- |
-- @match e1 e2@ tests whether @e1@ matches @e2@.
-- The term "match" stems from "signature matching" of module systems.
-- "Signature matching" is described, for example, in
-- "Advanced Topics in Types and Programming Languages" (Pierce, editor), Chapter 8.
match :: Env -> Env -> Bool
match = (==)

sfree :: Word32 -> TypeChecker ()
sfree w = do
  s <- lift $ gets stack
  guard $ w <= genericLength s
  putStack $ genericDrop w s

putStack :: Stack -> TypeChecker ()
putStack = lift . modify . mapStack . const
