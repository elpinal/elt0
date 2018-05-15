module Main where

import Control.Monad
import Data.ByteString.Builder
import System.Environment
import System.IO

import ELT0.Asm
import ELT0.Parser
import ELT0.Program

main :: IO ()
main = process

process :: IO ()
process = getArgs >>= g
  where
    g ("fmt" : xs) = f xs
    g ("asm" : xs) = asm xs
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
    f (Right p) = withFile o WriteMode $ flip hPutBuilder $ assemble p
    f (Left e) = hPutStrLn stderr $ show e
asm xs = fail $ "the number of arguments must be 2, but got " ++ show (length xs)
