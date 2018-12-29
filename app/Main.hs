module Main where

import Prelude.Compat

import qualified Command.Compile as Compile

import qualified Options.Applicative as Opts
import qualified System.IO as IO
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

import Data.Foldable (fold)
import Data.Monoid ((<>))
import System.Environment (getArgs)

-- | hardcoded for now
version :: String
version = "0.1.0"

main :: IO ()
main = do
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8
  cmd <- Opts.handleParseResult . execParserPure opts =<< getArgs
  cmd
  where
    opts = Opts.info (versionInfo <*> Opts.helper <*> commands) infoModList
    infoModList = Opts.fullDesc <> header <> footerInfo
    header = Opts.progDesc "Purescript -> Clojure backend compiler"
    footerInfo = Opts.footerDoc (Just footer)
    footer = mconcat [ Doc.hardline
                     , Doc.text $ "pursclj " ++ version ]

    versionInfo :: Opts.Parser (a -> a)
    versionInfo = Opts.abortOption (Opts.InfoMsg version) $
      Opts.long "version" <> Opts.short 'v' <> Opts.help "Show the version number" <> Opts.hidden

    execParserPure :: Opts.ParserInfo a -> [String] -> Opts.ParserResult a
    execParserPure pinfo [] = Opts.Failure $
      Opts.parserFailure Opts.defaultPrefs pinfo Opts.ShowHelpText mempty
    execParserPure pinfo args = Opts.execParserPure Opts.defaultPrefs pinfo args

    commands :: Opts.Parser (IO ())
    commands =
      (Opts.subparser . fold)
      [ Opts.command "compile"
        (Opts.info Compile.command
         (Opts.progDesc "Compile PureScript source files"))]
