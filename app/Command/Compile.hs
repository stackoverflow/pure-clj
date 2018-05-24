{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Command.Compile where

import Prelude.Compat

import qualified Options.Applicative as Opts

import Control.Applicative (many)
import Control.Monad
import Data.Monoid ((<>))
import Data.Text (Text)
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.UTF8 (readFileUTF8)
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
compile opts@PurscljMakeOptions{..} = do
  let pars = "compile" : pcljInput ++ ["-o", pcljOutputDir </> "_purs"] ++ ["-g", "corefn"]
      pars' = if pcljJSONErrors then pars ++ ["--json-errors"] else pars
  putStrLn $ show pars'
  (excode, out, err) <- readProcessWithExitCode purs pars' ""
  case excode of
    ExitSuccess -> do
      putStrLn "Compiling .purs files"
      putStrLn out
      putStrLn "Done"
      putStrLn "Compiling CoreFn to Clojure"
      compileClj opts
    ExitFailure code -> do
      putStrLn $ "purs process failed: " ++ show code
      putStrLn err

compileClj :: PurscljMakeOptions -> IO ()
compileClj PurscljMakeOptions{..} = do
  absPath <- makeAbsolute $ pcljOutputDir </> "_purs"
  putStrLn $ "output " ++ absPath
  corefns <- findCoreFns absPath
  modules <- readInput corefns
  putStrLn $ show corefns

compileCoreFn :: FilePath -> IO ()
compileCoreFn path = undefined

readInput :: [FilePath] -> IO [(FilePath, Text)]
readInput inputFiles = forM inputFiles $ \inFile -> (inFile, ) <$> readFileUTF8 inFile

findCoreFns :: FilePath -> IO [FilePath]
findCoreFns output = do
  simplePaths <- listDirectory output
  let paths = map (output </>) simplePaths
  dirs <- filterM doesDirectoryExist paths
  let corefns = map (</> "corefn.json") dirs
  return corefns

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
