{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Command.Compile where

import Prelude.Compat

import qualified Options.Applicative as Opts

import Clojure.Parser (parseClojure, hasForeign)
import Control.Applicative (many)
import Control.Monad
import Data.Monoid ((<>))
import Data.List (intercalate, isSuffixOf)
import Data.Text (Text, unpack)
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>), takeDirectory, dropExtension)
import System.FilePath.Glob (glob)
import System.IO.UTF8 (readFileUTF8, writeFileUTF8, readFileUTF8asString, writeFileUTF8asString)
import System.Process (readProcessWithExitCode)

import PureClj.Build
import CoreFn

-- | `purs` should be in the path
purs :: FilePath
purs = "purs"

data PurscljMakeOptions = PurscljMakeOptions
  { pcljInput         :: [FilePath]
  , pcljPursOutputDir :: FilePath
  , pcljOutputDir     :: FilePath
  , pcljJSONErrors    :: Bool
  }

compile :: PurscljMakeOptions -> IO ()
compile opts@PurscljMakeOptions{..} = do
  let pars = "compile" : pcljInput ++ ["-o", pcljPursOutputDir] ++ ["-g", "corefn"]
      pars' = if pcljJSONErrors then pars ++ ["--json-errors"] else pars
  putStrLn "Compiling .purs files"
  (excode, out, err) <- readProcessWithExitCode purs pars' ""
  case excode of
    ExitSuccess -> do
      putStrLn out
      putStrLn "Done"
      putStrLn "Compiling CoreFn to Clojure"
      compileClj opts
    ExitFailure code -> do
      putStrLn $ "purs process failed: " ++ show code
      putStrLn err

compileClj :: PurscljMakeOptions -> IO ()
compileClj PurscljMakeOptions{..} = do
  corefns <- findCoreFns pcljPursOutputDir
  modules <- readInput corefns
  let cljs = compileCoreFns modules
  forM_ cljs $ \(module', clj) -> do
    let mn = (runModuleNamePath $ moduleName module') ++ ["_core.clj"]
        file = foldl (</>) pcljOutputDir mn
    processForeigns module' pcljInput pcljOutputDir
    putStrLn $ "Writing " ++ (intercalate "." mn)
    writeOutput file clj

processForeigns :: Module Ann -> [FilePath] -> FilePath -> IO ()
processForeigns m@Module{..} inputDirs outDir =
  case moduleForeign of
    [] -> return ()
    _ -> do
      let module' = runModuleNamePath moduleName
      globs <- mapM glob inputDirs
      mods <- mapM (foreignExists m) $ concat globs
      let fpaths = foldl (\acc mb -> case mb of
                             Just fp -> acc ++ [fp]
                             Nothing -> acc) [] mods
      case fpaths of
        [] -> error $ "No foreign file found for module " ++ (intercalate "." module')
        [path] -> handleForeign path module'
        -- this is a rare case where one module
        -- is a suffix of another.
        xs -> handleForeign (chooseFittestModule modulePath xs) module'
  where
    chooseFittestModule :: FilePath -> [FilePath] -> FilePath
    chooseFittestModule mpath paths = case filter (== foreignPath) paths of
      [] -> error "Absurd: bug in function `choosefittestmodule`"
      (x:_) -> x
      where
        foreignPath = dropExtension mpath <.> "clj"
    handleForeign :: FilePath -> [FilePath] -> IO ()
    handleForeign path module' = do
      content <- readFileUTF8asString path
      let parsed = parseClojure content
          mname = intercalate "." module'
      case parsed of
        Left e -> error $ "Could not parse foreign module " ++ mname ++ " path: " ++ path ++ ":\n"  ++ (show e)
        Right cljs -> do
          let foreigns = map (unpack . runIdent) moduleForeign
              ffails = filter (not . hasForeign cljs) foreigns
          case ffails of
            [] -> do let out = foldl (</>) outDir $ module' ++ ["_foreign.clj"]
                     putStrLn $ "Writing foreign " ++ out
                     writeStringOutput out content
            fails -> error $ "Could not find foreign definitions: " ++ (intercalate " " fails)
                             ++ " for module: " ++ mname ++ " path: " ++ path

foreignExists :: Module Ann -> FilePath -> IO (Maybe FilePath)
foreignExists Module{..} input = do
  let mn = runModuleNamePath moduleName
      foreignFile = foldl1 (</>) mn
      input' = dropExtension input
  case isSuffixOf foreignFile input' of
    True -> do
      let file = input' <.> "clj"
      exists <- doesFileExist file
      if exists
        then return $ Just file
        else return Nothing
    _ -> return Nothing

writeStringOutput :: FilePath -> String -> IO ()
writeStringOutput path content = do
  mkdir path
  writeFileUTF8asString path content
  where
    mkdir :: FilePath -> IO ()
    mkdir = createDirectoryIfMissing True . takeDirectory

writeOutput :: FilePath -> Text -> IO ()
writeOutput path content = do
  mkdir path
  writeFileUTF8 path content
  where
    mkdir :: FilePath -> IO ()
    mkdir = createDirectoryIfMissing True . takeDirectory

readInput :: [FilePath] -> IO [(FilePath, Text)]
readInput files = forM files $ \file -> (file, ) <$> readFileUTF8 file

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
  <> Opts.long "clj-output"
  <> Opts.value "clj-output"
  <> Opts.showDefault
  <> Opts.help "The output directory"

pursOutputDirectory :: Opts.Parser FilePath
pursOutputDirectory = Opts.strOption $
     Opts.short 'p'
  <> Opts.long "output"
  <> Opts.value "output"
  <> Opts.showDefault
  <> Opts.help "The purs compiler output directory"

jsonErrors :: Opts.Parser Bool
jsonErrors = Opts.switch $
     Opts.long "json-errors"
  <> Opts.help "Print purs errors to stderr as JSON"

purscljMakeOptions :: Opts.Parser PurscljMakeOptions
purscljMakeOptions =
      PurscljMakeOptions
  <$> many inputFile
  <*> pursOutputDirectory
  <*> outputDirectory
  <*> jsonErrors

command :: Opts.Parser (IO ())
command = compile <$> (Opts.helper <*> purscljMakeOptions)
