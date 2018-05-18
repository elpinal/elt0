module Main where

import Control.Monad
import Data.Array.IO
import Data.ByteString.Builder
import Data.Word
import System.Environment
import System.IO

import ELT0.Asm.Refined
import ELT0.Eval (run)
import ELT0.Parser
import ELT0.Program

main :: IO ()
main = process

process :: IO ()
process = getArgs >>= g
  where
    g ("fmt" : xs) = f xs
    g ("asm" : xs) = asm xs
    g ("eval" : xs) = eval xs
    g (x : _) = fail $ "no such command: " ++  show x
    g [] = fail "no command specified"

    f []   = repl
    f args = mapM_ runFile args

repl :: IO ()
repl = forever $ do
  prompt
  stringify . mainParser <$> getLine >>= putStr

runFile :: String -> IO ()
runFile name = stringify . mainParser <$> readFile name >>= putStr

prompt :: IO ()
prompt = do
  putStr "> "
  hFlush stdout

stringify :: (Show a, Display b) => Either a b -> String
stringify (Right p) = display p
stringify (Left e)  = show e ++ "\n"

asm :: [String] -> IO ()
asm [o, i] = mainParser <$> readFile i >>= f
  where
    f (Right p) = withFile o WriteMode $ flip hPut $ assemble p
    f (Left e) = hPutStrLn stderr $ show e
asm xs = fail $ "the number of arguments must be 2, but got " ++ show (length xs)

eval :: [String] -> IO ()
eval [i] = withFile i ReadMode f
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
eval xs = fail $ "the number of arguments must be 1, but got " ++ show (length xs)
