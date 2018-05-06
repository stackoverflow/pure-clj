module Main where

import CoreFn

main :: IO ()
main = do
  contents <- readFile "test/resources/corefn_simple.json"
  print contents
