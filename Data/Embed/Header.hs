{-# LANGUAGE BangPatterns #-}
-- | The bundle data type, representing a binary bundle attached to an
--   executable.
module Data.Embed.Header where
import Control.Monad
import qualified Data.ByteString as BS
import Data.ByteString.UTF8
import Data.Hashable
import qualified Data.IntMap.Strict as M
import Data.Serialize
import Data.Word

type FileMap = M.IntMap [(FilePath, (Word64, Word32))]

-- | "Magic number" identifying a structure as a bundle header: "BNDLLDNB"
bundleMagicNumber :: Word64
bundleMagicNumber = 0x424e444c4c444e42 -- "BNDLLDNB"

-- | Current version of the bundle format.
bundleCurrentVersion :: Word8
bundleCurrentVersion = 0

-- | Maximum file size of a file in the bundle.
bundleMaxFileSize :: Integer
bundleMaxFileSize = fromIntegral (maxBound :: Word32)

-- | Size of the bundle's static header: two Word64 + two Word32 + one Word8.
bundleHeaderStaticSize :: Integer
bundleHeaderStaticSize = 25

-- | The static header of a file bundle.
data StaticHeader = StaticHeader {
    -- | The offset from the end of the file to the beginning of the file data
    --   section.
    hdrDataOffset  :: !Word64,

    -- | Total number of files in the bundle.
    hdrNumFiles    :: !Word32,

    -- | Size of the dynamic part of the header, where file names, offsets and
    --   sizes are stored.
    hdrDynSize     :: !Word32,

    -- | Version of the bundle structure. Must always be less than or equal to
    --   'bundleCurrentVersion'
    hdrVersion     :: !Word8,

    -- | Magic number. Must always be 'bundleMagicNumber'.
    hdrMagicNumber :: !Word64
  }

-- | A file bundle header. Contains basic metadata about the bundle itself, and
--   the metadata for all files contained therein.
data BundleHeader = BundleHeader {
    -- | A hash map from file paths to the respective offsets and sizes.
    hdrFiles      :: !FileMap,
    
    -- | Static part of the header.
    hdrStatic     :: !StaticHeader
  }

-- | Build a static header with the specified number of files, size of the
--   dynamic header, and offset to the start of the data section.
mkStaticHdr :: Int -> Int -> Word64 -> StaticHeader
mkStaticHdr nfiles dynsize dataoff = StaticHeader {
    hdrVersion     = bundleCurrentVersion,
    hdrMagicNumber = bundleMagicNumber,
    hdrNumFiles    = fromIntegral nfiles,
    hdrDynSize     = fromIntegral dynsize,
    hdrDataOffset  = dataoff
  }

instance Serialize StaticHeader where
  put hdr = do
    putWord64le (hdrDataOffset hdr)
    putWord32le (hdrNumFiles hdr)
    putWord32le (hdrDynSize hdr)
    putWord8 (hdrVersion hdr)
    putWord64le (hdrMagicNumber hdr)
  get =
    StaticHeader <$> getWord64le
                 <*> getWord32le
                 <*> getWord32le
                 <*> getWord8
                 <*> getWord64le

getHdrFile :: Get (FilePath, (Word64, Word32))
getHdrFile = do
  pathlen <- getWord32le
  path <- getBytes (fromIntegral pathlen)
  off <- getWord64le
  sz <- getWord32le
  return (toString path, (off, sz))

getHdrFiles :: Word32 -> Get FileMap
getHdrFiles = getFiles M.empty
  where
    getFiles !m 0 = pure m
    getFiles !m n = do
      f <- getHdrFile
      getFiles (M.alter (ins f) (hash $ fst f) m) (n-1)
    ins f (Just fs) = Just (f:fs)
    ins f _         = Just [f]

putHdrFiles :: FileMap -> BS.ByteString
putHdrFiles m = runPut $ forM_ (concat $ M.elems m) $ \(p, (off, sz)) -> do
  let p' = fromString p
  putWord32le (fromIntegral $ BS.length p')
  putByteString p'
  putWord64le off
  putWord32le sz
