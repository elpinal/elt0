module Main
  ( main
  ) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Data.Array.IO
import qualified Data.Map.Lazy as Map
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

instance Exception CommandException

instance Show CommandException where
  show (ParseException p e)   = show p ++ ": parse error: " ++ show e
  show (ArgNumException e g)  = "the number of arguments must be " ++ show e ++ ", but got " ++ show g
  show (TypeCheckException p) = "ill-typed: " ++ show p
  show (NoCommand s)          = "no such command: " ++ show s
  show (NoPrimitive s)        = "no such primitive: " ++ show s

main :: IO ()
main = getArgs >>= process

type CommandTable a = Map.Map String ([String] -> a ())

process :: (MonadIO m, MonadThrow m) => [String] -> m ()
process = switch commands NoCommand

primitive :: (MonadIO m, MonadThrow m) => [String] -> m ()
primitive = switch prims NoPrimitive

commands :: (MonadIO m, MonadThrow m) => CommandTable m
commands = Map.fromList
  [ ("build", build)
  , ("eval", eval)
  , ("fmt", fmt)
  , ("help", help)
  , ("primitive", primitive)
  ]

prims :: (MonadIO m, MonadThrow m) => CommandTable m
prims = Map.fromList
  [ ("asm", asm)
  , ("typecheck", typecheck)
  ]

switch :: (MonadIO m, MonadThrow m) => CommandTable m -> (String -> CommandException) -> [String] -> m ()
switch t e (x : xs) = Map.findWithDefault (const . throwM $ e x) x t xs
switch t _ []       = liftIO $ mapM_ putStrLn $ Map.keys t

help :: (MonadIO m, MonadThrow m) => [String] -> m ()
help [] = liftIO $ mapM_ putStrLn $ Map.keys (commands :: CommandTable IO)
help xs = argMismatch 0 xs

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

build :: (MonadIO m, MonadThrow m) => [String] -> m ()
build [o, i] = readAsm i >>= f
  where
    f p = case program p $ fromProgram p of
      Just () -> liftIO $ withFile o WriteMode $ flip hPut $ assemble p
      Nothing -> throwM $ TypeCheckException i
build xs = argMismatch 2 xs

argMismatch :: MonadThrow m => Int -> [a] -> m ()
argMismatch n = throwM . ArgNumException n . length

readAsm :: (MonadIO m, MonadThrow m) => FilePath -> m Program
readAsm p = liftIO (readFile p) >>= either (throwM . ParseException p) return . fromString
