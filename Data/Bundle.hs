-- | This module provides access to files bundled with the current executable.
module Data.Bundle where
import Data.Serialize

bundledFile :: Binary a => FilePath -> Maybe a
bundledFile
