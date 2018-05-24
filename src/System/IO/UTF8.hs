module System.IO.UTF8 where

import Prelude.Compat

import Data.Text (Text)

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE

readFileUTF8 :: FilePath -> IO Text
readFileUTF8 file = fmap TE.decodeUtf8 (BS.readFile file)
