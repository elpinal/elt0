module Main where

import ELT0.Parser
import ELT0.Program

main :: IO ()
main = interact process

process :: String -> String
process s = case mainParser s of
  Right p -> display p
  Left e -> show e
