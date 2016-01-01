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
-- | Handle to this application's embedded file bundle.
--   Throws an error upon evaluation if the application has none.
myBundle :: Bundle
myBundle =
  case unsafePerformIO (getExecutablePath >>= openBundle) of
    Right b -> b
    Left e  -> error $ "couldn't open executable's bundle: " ++ e

-- | Returns the specified embedded file, or Nothing if it does not exist or
--   this executable has no embedded file bundle.
embeddedFile :: FilePath -> Maybe BS.ByteString
embeddedFile fp =
    case theFile of
      Right bytes -> Just bytes
      _           -> Nothing
  where
    theFile = unsafePerformIO $ do
      eb <- getExecutablePath >>= openBundle
      case eb of
        Right b -> readBundleFile b fp
        Left e  -> return (Left e)

-- | Like 'embeddedFile', but throws an error if the given file does not exist
--   in the executable's embedded file bundle.
embeddedFile' :: FilePath -> BS.ByteString
embeddedFile' fp =
  maybe (error $ "no such embedded file: `" ++ fp ++ "'") id $ embeddedFile fp
