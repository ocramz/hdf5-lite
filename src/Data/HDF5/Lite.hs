
{- |
Module      : Data.HDF5.Lite
Copyright   : (c) Marco Zocca, 2018
License     : GPL-3
Maintainer  : zocca.marco gmail
Stability   : experimental
Portability : POSIX
= Introduction

This module exposes the main interface to @hdf5-lite@.

-}
module Data.HDF5.Lite (
  -- * File API
  withFile
  , withFileCreate
  , FileOpenMode(..)  
  -- -- * Dataspace API
  -- , withDataspace
  -- * Dataset API
  , StorableDataset(..)
  -- -- ** Create
  -- , makeDatasetFloat
  -- , makeDatasetDouble
  -- -- ** Read
  -- , readDatasetFloat
  -- , readDatasetDouble
  -- ** Information
  , getDatasetInfo
  -- * HDF5 Types
  , Hid, Hsize, Herr, H5T_class_t
  -- * Exceptions
  , HDF5Exception
                      ) where

import Data.Functor ((<$>))

import Control.Monad.Catch (throwM, MonadThrow(..))
import Control.Exception (Exception(..), bracket)

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt, CChar, CDouble, CFloat, CSize)
-- import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray, peekArray, lengthArray0)
import Foreign.ForeignPtr (ForeignPtr(..), mallocForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable

import GHC.Prim

import qualified Language.C.Inline as C

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS (Vector(..), unsafeWith, unsafeFromForeignPtr0, unsafeToForeignPtr0, fromList)
import qualified Data.Vector.Storable.Mutable as VM

import Data.HDF5.Internal.Types (Herr, Hid, Hsize, H5T_class_t)
import Data.HDF5.Lite.Internal.InlineC
import Data.HDF5.Internal.Exceptions
import Data.HDF5.Store



-- * File (H5F)

-- | File memory bracket, preexisting file
withFile :: String -> FileOpenMode -> (Hid -> IO c) -> IO c
withFile name mode = bracket (fopen name mode) fclose


-- | File memory bracket
withFileCreate :: String -> (Hid -> IO c) -> IO c
withFileCreate name = bracket (fcreate name) fclose


-- * Dataspace (H5S)

-- | Memory bracket for dataspaces
withDataspace :: CInt -> [Hsize] -> (Hid -> IO c) -> IO c
withDataspace rank dims = bracket (screateSimple rank dims) sclose




-- * Dataset (H5D)

makeDatasetDouble
  :: Hid -> String -> [Hsize] -> VS.Vector C.CDouble -> IO ()
makeDatasetDouble loc name dims buffer =
  withMakeDataset loc name dims buffer makeDatasetDouble'


-- float

makeDatasetFloat
  :: Hid -> String -> [Hsize] -> VS.Vector CFloat -> IO ()
makeDatasetFloat loc name dims buffer =
  withMakeDataset loc name dims buffer makeDatasetFloat'

-- int

makeDatasetInt
  :: Hid -> String -> [Hsize] -> VS.Vector CInt -> IO ()
makeDatasetInt loc name dims bp =
  withMakeDataset loc name dims bp makeDatasetInt'
  

-- | Helper function for dataset creation
withMakeDataset :: (Num t1, Storable a1, Storable a2) =>
     t2
     -> String
     -> [a1]
     -> VS.Vector a2
     -> (t2 -> CString -> t1 -> Ptr a1 -> Ptr a2 -> IO Herr)
     -> IO ()
withMakeDataset loc name dims buffer io =
    let
    rank = fromIntegral $ length dims
  in 
    withCString name $ \name_ ->
    withArray dims $ \dims_ -> 
    VS.unsafeWith buffer $ \ bp_ -> throwH (io loc name_ rank dims_ bp_)

instance StorableDataset CFloat where
  makeDataset = makeDatasetFloat
  readDataset = readDatasetFloat

instance StorableDataset CDouble where
  makeDataset = makeDatasetDouble
  readDataset = readDatasetDouble


-- herr_t H5LTmake_dataset_char ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const char *buffer )



readDatasetDouble :: Hid -> String -> IO (VS.Vector CDouble)
readDatasetDouble loc name = withReadDataset loc name (undefined :: CDouble) readDatasetDouble'  


readDatasetFloat :: Hid -> String -> IO (VS.Vector CFloat)
readDatasetFloat loc name = withReadDataset loc name (undefined :: CFloat) readDatasetFloat'  


withReadDataset
  :: (Storable a, Eq a) =>
     t
     -> String
     -> a
     -> (t -> CString -> Ptr a -> IO Herr)
     -> IO (VS.Vector a)
withReadDataset loc name x io =
  fst <$> withCString name ( \name_ ->
  peekVS x $ \bp -> throwH (io loc name_ bp))










  
