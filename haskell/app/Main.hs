module Main where

import Control.Monad
import System.Environment
import System.IO

import ELT0.Parser
import ELT0.Program

main :: IO ()
main = process

process :: IO ()
process = do
  args <- getArgs
  if null args
    then repl
    else mapM_ runFile args

repl :: IO ()
repl = forever $ do
  putStr "> "
  hFlush stdout
  s <- getLine
  putStr $ case mainParser s of
    Right p -> display p
    Left e -> show e ++ "\n"

runFile :: String -> IO ()
runFile name = do
  s <- readFile name
  putStr $ case mainParser s of
    Right p -> display p
    Left e -> show e ++ "\n"
