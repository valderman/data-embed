{-# LANGUAGE RecordWildCards #-}
-- | Reading/writing bundles of embedded files and other data from/to
--   executables.
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
--
--   The included @bundletool@ program offers a command line interface
--   for manipulating and inspecting bundles.
module Data.Embed.File (
    Bundle,

    -- * Reading bundles
    hasBundle, openBundle, withBundle, closeBundle, readBundleFile, readBundle,
    listBundleFiles,

    -- * Creating bundles
    File (..), appendBundle, eraseBundle, replaceBundle
  ) where
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.Hashable
import qualified Data.IntMap as M
import Data.Serialize
import Data.String
import System.Directory
import System.IO
import Data.Embed.Header

-- | A file to be included in a bundle. May be either the path to a file on
--   disk or an actual (path, data) pair.
--
--   If a file path refers to a directory, all non-dotfile files and
--   subdirectories of that directory will be included in the bundle.
--   File paths also have a strip number: the number of leading directories
--   to strip from file names when adding them to a bundle.
--   For instance, adding @File 1 "foo/bar"@ to a bundle will add the file
--   @foo/bar@, under the name @bar@ within the bundle.
--
--   If a file name would "disappear" entirely due to stripping, for instance
--   when stripping two directories from @foo/bar@, @bar@ will "disappear"
--   entirely and so will be silently ignored.
data File = FilePath Int FilePath | FileData FilePath BS.ByteString

instance IsString File where
  fromString = FilePath 0

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

-- | Read a bundle static header from the end of a file.
readStaticHeader :: Handle -> IO (Either String StaticHeader)
readStaticHeader hdl = do
  hSeek hdl SeekFromEnd (negate bundleHeaderStaticSize)
  ehdr <- decode <$> BS.hGet hdl (fromInteger bundleHeaderStaticSize)
  case ehdr of
    Left e ->
        pure (Left $ "unable to parse static header: " ++ e)
    Right hdr
      | hdrMagicNumber hdr /= bundleMagicNumber ->
        pure (Left "not a bundle")
      | hdrVersion hdr > bundleCurrentVersion ->
        pure (Left "unsupported bundle version")
      | otherwise ->
        pure (Right hdr)

-- | Open a file bundle. The bundle will keep an open handle to its backing
--   file. The handle will be closed when the bundle is garbage collected.
--   Use 'closeBundle' to close the handle before the 
openBundle :: FilePath -> IO (Either String Bundle)
openBundle fp = flip catch (\(SomeException e) -> pure (Left $ show e)) $ do
  -- Read static header
  hdl <- openBinaryFile fp ReadMode
  ehdr <- readStaticHeader hdl
  case ehdr of
    Left err        -> pure (Left err)
    Right statichdr -> do
      -- Read dynamic header
      let dynsize = fromIntegral (hdrDynSize statichdr)
          dynoffset = bundleHeaderStaticSize + dynsize
      hSeek hdl SeekFromEnd (negate dynoffset)
      bytes <- BS.hGet hdl (fromInteger dynsize)

      -- Parse dynamic header and create bundle
      case runGet (getHdrFiles (hdrNumFiles statichdr)) bytes of
        Left e        -> fail $ "unable to parse file list: " ++ e
        Right filehdr -> do
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

-- | Write a bundle to a file. If the given file already has a bundle, the new
--   bundle will be written *after* the old one. The old bundle will thus still
--   be present in the file, but only the new one will be recognized by
--   'openBundle' and friends.
appendBundle :: FilePath -> [File] -> IO ()
appendBundle fp fs = withBinaryFile fp AppendMode $ \hdl -> do
    (datasize, metadata) <- foldM (packFile hdl) (0, M.empty) fs
    let mdbytes = putHdrFiles metadata
        mdlen   = BS.length mdbytes
        dataoff = datasize + fromIntegral mdlen
                           + fromIntegral bundleHeaderStaticSize 
        hdr = mkStaticHdr (M.size metadata) mdlen dataoff
    BS.hPut hdl mdbytes
    BS.hPut hdl (encode hdr)
  where
    isDotFile ('.':_) = True
    isDotFile _       = False

    p </> f = p ++ "/" ++ f

    stripLeading 0 f = f
    stripLeading n f = stripLeading (n-1) (drop 1 (dropWhile (/= '/') f))

    packFile hdl acc (FilePath n p) = do
      let stripped = stripLeading n p
      if null stripped
        then return acc
        else do
          isDir <- doesDirectoryExist p
          if isDir
            then do
              files <- filter (not . isDotFile) <$> getDirectoryContents p
              foldM (packFile hdl) acc (map (FilePath n . (p </>)) files)
            else BS.readFile p >>= packFile hdl acc . FileData stripped
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

-- | List all files in the given bundle. Will succeed even on closed bundles.
listBundleFiles :: Bundle -> [FilePath]
listBundleFiles = map fst . concat . M.elems . hdrFiles . bundleHeader

-- | Like 'readBundleFile', but attempts to decode the file's contents into an
--   appropriate Haskell value.
readBundle :: Serialize a => Bundle -> FilePath -> IO (Either String a)
readBundle b fp = do
  ebytes <- readBundleFile b fp
  pure (ebytes >>= decode)

-- | Does the given file contain a bundle or not?
hasBundle :: FilePath -> IO Bool
hasBundle fp = do
  ehdr <- withBinaryFile fp ReadMode readStaticHeader
  case ehdr of
    Right _ -> pure True
    _       -> pure False

-- | Remove a bundle from an existing file. Does nothing if the given file
--   does not have a bundle. The given file is *not* removed, even if it only
--   contains the bundle.
eraseBundle :: FilePath -> IO ()
eraseBundle fp = do
  withBinaryFile fp ReadWriteMode $ \hdl -> do
    ehdr <- readStaticHeader hdl
    case ehdr of
      Right hdr -> do
        sz <- hFileSize hdl
        hSetFileSize hdl (sz - fromIntegral (hdrDataOffset hdr))
      _         -> return ()

-- | Replace the bundle currently attached to the given file. Equivalent to
--   'appendBundle' if the given file does not already have a bundle attached.
replaceBundle :: FilePath -> [File] -> IO ()
replaceBundle fp fs = eraseBundle fp >> appendBundle fp fs
