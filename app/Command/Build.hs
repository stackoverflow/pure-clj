{-# LANGUAGE RecordWildCards #-}

module Command.Build where

import Prelude.Compat

import qualified Options.Applicative as Opts

import Control.Applicative (many)
import Data.Monoid ((<>))
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

-- | `purs` should be in the path
purs :: FilePath
purs = "purs"

data PurscljMakeOptions = PurscljMakeOptions
  { pcljInput        :: [FilePath]
  , pcljOutputDir    :: FilePath
  , pcljJSONErrors   :: Bool
  }

compile :: PurscljMakeOptions -> IO ()
compile PurscljMakeOptions{..} = do
  let pcljInputStr = show <$> pcljInput
      isJsonErrors = show pcljJSONErrors
  (excode, out, err) <- readProcessWithExitCode purs (pcljInputStr ++ ["-o " ++ pcljOutputDir] ++ ["--json-errors " ++ isJsonErrors]) ""
  case excode of
    ExitSuccess -> putStrLn out
    ExitFailure f -> do
      putStrLn $ "failed with code " ++ show f
      putStrLn err
  putStrLn "done"

inputFile :: Opts.Parser FilePath
inputFile = Opts.strArgument $
  Opts.metavar "FILE"
  <> Opts.help "The input .purs file(s)."

outputDirectory :: Opts.Parser FilePath
outputDirectory = Opts.strOption $
  Opts.short 'o'
  <> Opts.long "output"
  <> Opts.value "output"
  <> Opts.showDefault
  <> Opts.help "The output directory"

jsonErrors :: Opts.Parser Bool
jsonErrors = Opts.switch $
  Opts.long "json-errors"
  <> Opts.help "Print errors to stderr as JSON"

purscljMakeOptions :: Opts.Parser PurscljMakeOptions
purscljMakeOptions = PurscljMakeOptions
  <$> many inputFile
  <*> outputDirectory
  <*> jsonErrors

command :: Opts.Parser (IO ())
command = compile <$> (Opts.helper <*> purscljMakeOptions)
