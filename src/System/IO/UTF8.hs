module System.IO.UTF8 where

import Prelude.Compat

import Data.Text (Text)

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSUTF8
import qualified Data.Text.Encoding as TE

readFileUTF8 :: FilePath -> IO Text
readFileUTF8 file = fmap TE.decodeUtf8 (BS.readFile file)

writeFileUTF8 :: FilePath -> Text -> IO ()
writeFileUTF8 path text = BS.writeFile path (TE.encodeUtf8 text)

readFileUTF8asString :: FilePath -> IO String
readFileUTF8asString file = fmap BSUTF8.toString (BS.readFile file)
