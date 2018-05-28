module Main where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Data.Array.IO
import Data.Word
import System.Environment
import System.IO

import ELT0.Asm
import ELT0.Eval (run)
import ELT0.Parser
import ELT0.Program
import ELT0.Type

data CommandException
  = ParseException FilePath Error
  | ArgNumException Int Int
  | TypeCheckException FilePath
  | NoCommand String
  | NoPrimitive String
  deriving Show

instance Exception CommandException

main :: IO ()
main = process

process :: (MonadIO m, MonadThrow m) => m ()
process = liftIO getArgs >>= f
  where
    f ("eval" : xs)      = eval xs
    f ("fmt" : xs)       = fmt xs
    f ("primitive" : xs) = primitive xs
    f (x : _)            = throwM $ NoCommand x
    f []                 = liftIO $ mapM_ putStrLn ["eval", "fmt", "primitive"]

primitive :: (MonadIO m, MonadThrow m) => [String] -> m ()
primitive ("asm" : xs)       = asm xs
primitive ("typecheck" : xs) = typecheck xs
primitive (x : _)            = throwM $ NoPrimitive x
primitive []                 = liftIO $ mapM_ putStrLn ["asm", "typecheck"]

typecheck :: (MonadIO m, MonadThrow m) => [String] -> m ()
typecheck [i] = readAsm i >>= f
  where
    f p = case program p $ fromProgram p of
      Just () -> liftIO $ putStrLn "Successfully typechecked."
      Nothing -> throwM $ TypeCheckException i
typecheck xs = argMismatch 1 xs

asm :: (MonadIO m, MonadThrow m) => [String] -> m ()
asm [o, i] = readAsm i >>= liftIO . withFile o WriteMode . flip hPut . assemble
asm xs = argMismatch 2 xs

fmt :: (MonadIO m, MonadThrow m) => [String] -> m ()
fmt [name] = readAsm name >>= liftIO . putStr . display
fmt xs     = argMismatch 1 xs

eval :: (MonadIO m, MonadThrow m) => [String] -> m ()
eval [i] = liftIO $ withFile i ReadMode f
  where
    f :: Handle -> IO ()
    f hdl = do
      size <- toInt <$> hFileSize hdl
      a <- newArray_ (1, size) -- TODO: what happens for size = 0?
      _ <- hGetArray hdl a size -- TODO: should we consider the returned size?
      a' <- mapIndices (1, toWord32 size) fromWord32 a >>= freeze
      print $ run (a', 1)

    -- FIXME: unsafe
    toInt :: Integer -> Int
    toInt = fromIntegral

    -- FIXME: unsafe
    toWord32 :: Int -> Word32
    toWord32 = fromIntegral

    -- FIXME: unsafe
    fromWord32 :: Word32 -> Int
    fromWord32 = fromIntegral
eval xs = argMismatch 1 xs

argMismatch :: MonadThrow m => Int -> [a] -> m ()
argMismatch n = throwM . ArgNumException n . length

readAsm :: (MonadIO m, MonadThrow m) => FilePath -> m Program
readAsm p = liftIO (readFile p) >>= either (throwM . ParseException p) return . fromString
