-- | This module provides access to files bundled with the current executable.
--   To bundle files within an executable, use the accompanying @bundletool@
--   command line tool, or the functions provided in "Data.Bundle.File".
--
--   To bundle a directory of data files with an executable, try:
--
--   > bundletool -p1 -w my_executable my_data_dir
module Data.Bundle where
import qualified Data.ByteString as BS
import System.IO.Unsafe
import System.Environment.Executable
import Data.Bundle.File

{-# NOINLINE myBundle #-}
-- | Handle to this application's bundle. Throws an error upon evaluation if
--   the application has no bundle.
myBundle :: Bundle
myBundle =
  case unsafePerformIO (getExecutablePath >>= openBundle) of
    Right b -> b
    Left e  -> error $ "couldn't open executable's bundle: " ++ e

-- | Returns the specified bundled file, or Nothing if it does not exist.
bundledFile :: FilePath -> Maybe BS.ByteString
bundledFile fp =
  case unsafePerformIO (readBundleFile myBundle fp) of
    Right bytes -> Just bytes
    _           -> Nothing

-- | Like 'bundledFile', but throws an error if the given file does not exist
--   in the executable's bundle.
bundledFile' :: FilePath -> BS.ByteString
bundledFile' fp =
  maybe (error $ "no such bundled file: `" ++ fp ++ "'") id $ bundledFile fp
