-- | This module provides access to files bundled with the current executable.
--   To bundle files within an executable, use the accompanying @embedtool@
--   command line tool, or the functions provided in "Data.Embed.File".
--
--   To embed a directory of data files with an executable, try:
--
--   > embedtool -p1 -w my_executable my_data_dir
module Data.Embed where
import qualified Data.ByteString as BS
import System.IO.Unsafe
import System.Environment.Executable
import Data.Embed.File

{-# NOINLINE myBundle #-}
-- | Handle to this application's bundle. Throws an error upon evaluation if
--   the application has no bundle.
myBundle :: Bundle
myBundle =
  case unsafePerformIO (getExecutablePath >>= openBundle) of
    Right b -> b
    Left e  -> error $ "couldn't open executable's bundle: " ++ e

-- | Returns the specified embedded file, or Nothing if it does not exist.
embeddedFile :: FilePath -> Maybe BS.ByteString
embeddedFile fp =
  case unsafePerformIO (readBundleFile myBundle fp) of
    Right bytes -> Just bytes
    _           -> Nothing

-- | Like 'bundledFile', but throws an error if the given file does not exist
--   in the executable's bundle.
embeddedFile' :: FilePath -> BS.ByteString
embeddedFile' fp =
  maybe (error $ "no such bundled file: `" ++ fp ++ "'") id $ embeddedFile fp
