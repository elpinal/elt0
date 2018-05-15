module Main where

import Control.Monad
import System.Environment
import System.IO

import ELT0.Parser
import ELT0.Program

main :: IO ()
main = process

process :: IO ()
process = getArgs >>= f
  where
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
