module Main where

import Prelude.Compat

import qualified Options.Applicative as Opts
import qualified System.IO as IO
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import System.Environment (getArgs)

main :: IO ()
main = do
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8
  putStrLn "Not yet"
