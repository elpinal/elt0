module Main where

import Control.Monad
import System.IO

import ELT0.Parser
import ELT0.Program

main :: IO ()
main = process

process :: IO ()
process = forever $ do
  putStr "> "
  hFlush stdout
  s <- getLine
  putStr $ case mainParser s of
    Right p -> display p
    Left e -> show e ++ "\n"
