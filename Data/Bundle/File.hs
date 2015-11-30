{-# LANGUAGE RecordWildCards #-}
-- | Reading and writing executable bundles from/to files.
--
--   A bundle has three parts: the static header, which identifies
--   a string of bytes as a bundle using a particular version of the format
--   and gives the size of the dynamic header; the dynamic header which
--   describes all files and directories contained in the bundle; and the data
--   part, where the data for all files is located.
--   All words are stored in little endian format.
--
--   The static header comprises the last 'bundleHeaderStaticSize' bytes of the
--   file, with the dynamic header coming immediately before, and the data
--   section coming immediately before the dynamic header.
--
--   The dynamic header is stored as a tuple of the number of files in the
--   bundle (Word32), and the .
--   Each file is stored as a triple of the file's UTF-8 encoded path,
--   its offset from the start of the data section, and its size.
--   The path is prepended with a Word32 giving its length in bytes; the
--   offset is given as a Word32 and the size is given as a Word32.
--
--   The layout of the bundle format is given in the following table:
--
--   > [file data]
--   > 
--   > [files] * hdrNumFiles
--   >   pathLen       : Word32
--   >   path          : pathLen * Word8
--   >   offset        : Word64
--   >   size          : Word32
--   > 
--   > [static header]
--   >   hdrDataOffset : Word64
--   >   hdrNumFiles   : Word32
--   >   hdrDynSize    : Word32
--   >   hdrVersion    : Word8
--   >   "BNDLLDNB"    : Word64
module Data.Bundle.File (
    Bundle,

    -- * Reading bundles
    openBundle, withBundle, closeBundle, readBundleFile, readBundle,

    -- * Creating bundles
    File (..), writeBundle,
  ) where
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.Hashable
import qualified Data.IntMap as M
import Data.Serialize
import Data.String
import System.IO
import Data.Bundle.Header

-- | A file to be included in a bundle. May be either the path to a file on
--   disk or an actual (path, file) pair.
data File = FilePath FilePath | FileData FilePath BS.ByteString

instance IsString File where
  fromString = FilePath

-- | A handle to a file bundle. Bundle handles are obtained using 'openBundle'
--   or 'withBundle' and start out as open. That is, files may be read from
--   them. An open bundle contains an open file handle to the bundle's backing
--   file, ensuring that the file data will not disappear from under it.
--   When a bundle becomes unreachable, its corresponding file handle is
--   closed by the bundle's associated finalizer.
--
--   However, as finalizers are not guaranteed to run promptly - or even
--   at all - bundles may also be closed before becoming unreachable using
--   'closeBundle'. If you expect to perform other operations on a bundle's
--   backing file, you should always close the bundle manually first.
data Bundle = Bundle {
    -- | The header of the bundle.
    bundleHeader     :: !BundleHeader,

    -- | An open handle to the bundle's file, for reading file data.
    bundleHandle     :: !Handle,

    -- | The offset from the end of the file to the start of the data section.
    bundleDataOffset :: !Integer,

    -- | Lock to ensure thread safety of read/close operations. Iff 'True', then
    --   the bundle is still alive, i.e. the handle is still open.
    bundleLock       :: !(MVar Bool)
  }

-- | Open a file bundle. The bundle will keep an open handle to its backing
--   file. The handle will be closed when the bundle is garbage collected.
--   Use 'closeBundle' to close the handle before the 
openBundle :: FilePath -> IO (Either String Bundle)
openBundle fp = flip catch (\(SomeException e) -> pure (Left $ show e)) $ do
  -- Read static header
  hdl <- openBinaryFile fp ReadMode
  hSeek hdl SeekFromEnd (negate bundleHeaderStaticSize)
  ehdr <- decode <$> BS.hGet hdl (fromInteger bundleHeaderStaticSize)
  statichdr <- case ehdr of
    Right hdr -> pure hdr
    Left e    -> fail $ "unable to parse static header: " ++ e

  -- Sanity checks
  when (hdrMagicNumber statichdr /= bundleMagicNumber) $
    fail "not a bundle"
  when (hdrVersion statichdr > bundleCurrentVersion) $
    fail "unsupported bundle version"

  -- Read dynamic header
  let dynsize = fromIntegral (hdrDynSize statichdr)
      dynoffset = bundleHeaderStaticSize + dynsize
  hSeek hdl SeekFromEnd (negate dynoffset)
  bytes <- BS.hGet hdl (fromInteger dynsize)
  filehdr <- case runGet (getHdrFiles (hdrNumFiles statichdr)) bytes of
    Right hdr -> pure hdr
    Left e    -> fail $ "unable to parse file list: " ++ e
  let hdr = BundleHeader {
          hdrFiles = filehdr,
          hdrStatic = statichdr
        }
  lock <- newMVar True
  let b = Bundle hdr hdl (fromIntegral (hdrDataOffset statichdr)) lock
  _ <- mkWeakMVar lock (closeBundle b)
  pure $! Right $! b

-- | Close a bundle before it becomes unreachable. After a bundle is closed,
--   any read operations performed on it will fail as though the requested
--   file could not be found.
--   Subsequent close operations on it will have no effect.
closeBundle :: Bundle -> IO ()
closeBundle b = do
  alive <- takeMVar (bundleLock b)
  when alive $ hClose (bundleHandle b)
  putMVar (bundleLock b) False

-- | Perform a computation over a bundle, returning an error if either the
--   computation failed or the bundle could not be loaded.
--   The bundle is always closed before this function returns, regardless of
--   whether an error occurred.
withBundle :: FilePath -> (Bundle -> IO a) -> IO (Either String a)
withBundle fp f = do
    eb <- openBundle fp
    case eb of
      Right b -> fmap Right (f b) `catch` handler <* closeBundle b
      Left e  -> return (Left e)
  where
    handler (SomeException e) = pure (Left $ show e)

-- | Write a bundle to a file.
writeBundle :: FilePath -> [File] -> IO ()
writeBundle fp fs = withBinaryFile fp AppendMode $ \hdl -> do
    (datasize, metadata) <- foldM (packFile hdl) (0, M.empty) fs
    let mdbytes = putHdrFiles metadata
        mdlen   = BS.length mdbytes
        dataoff = datasize + fromIntegral mdlen
                           + fromIntegral bundleHeaderStaticSize 
        hdr = mkStaticHdr (M.size metadata) mdlen dataoff
    BS.hPut hdl mdbytes
    BS.hPut hdl (encode hdr)
  where
    packFile hdl acc (FilePath p) =
      BS.readFile p >>= packFile hdl acc . FileData p
    packFile hdl (off, m) (FileData p d) = do
      BS.hPut hdl d
      let len = fromIntegral (BS.length d)
          off' = off + fromIntegral len
      off' `seq` return (off', (M.alter (ins p off len) (hash p) m))

    ins p off len Nothing   = Just [(p, (off, len))]
    ins p off len (Just xs) = Just ((p, (off, len)) : xs)

-- | Read a file from a previously opened bundle. Will fail of the given path
--   is not found within the bundle, or if the bundle is no longer alive, i.e.
--   it has been closed using 'closeBundle'.
readBundleFile :: Bundle -> FilePath -> IO (Either String BS.ByteString)
readBundleFile (Bundle {..}) fp =
  flip catch (\(SomeException e) -> pure (Left $ show e)) $ do
    withMVar bundleLock $ \v -> do
      when (v == False) $ fail "bundle already closed"
      case M.lookup (hash fp) (hdrFiles bundleHeader) >>= lookup fp of
        Nothing        -> fail "no such file"
        Just (off, sz) -> do
          hSeek bundleHandle SeekFromEnd (fromIntegral off - bundleDataOffset)
          Right <$> BS.hGet bundleHandle (fromIntegral sz)

-- | Like 'readBundleFile', but attempts to decode the file's contents into an
--   appropriate Haskell value.
readBundle :: Serialize a => Bundle -> FilePath -> IO (Either String a)
readBundle b fp = do
  ebytes <- readBundleFile b fp
  pure (ebytes >>= decode)
