{-# language OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
{-# language TypeFamilies #-}
module Data.HDF5.Lite where

import           Data.Functor ((<$>))

import Control.Monad.Catch (throwM, MonadThrow(..))
import Control.Exception (bracket)

import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable

import GHC.Prim

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VM

import           Data.HDF5.Internal.Types (Herr, Hid, Hsize)
import qualified Data.HDF5.Lite.Internal as H
import qualified Language.C.Inline as C

import Data.HDF5.Internal.Exceptions

C.context H.hdf5ctx

C.include "<hdf5_hl.h>"


  

-- | HDF5 Lite API reference: https://support.hdfgroup.org/HDF5/doc/HL/RM_H5LT.html






-- * HDF5 file operations
-- NB: each HDF5 physical file (e.g. "/usr/data/dataset0.h5" ) specifies a whole internal filesystem, the elements of which are what we call "files" here

-- hid_t H5Fopen( const char *name, unsigned flags, hid_t fapl_id )
fopenReadOnly :: String -> IO Hid
fopenReadOnly name = withCString name $ \name_ -> 
  [C.exp| hid_t{ H5Fopen( $(const char* name_), H5F_ACC_RDONLY, H5P_DEFAULT )}|]

fopenRW :: String -> IO Hid
fopenRW name = withCString name $ \name_ ->
  [C.exp| hid_t{ H5Fopen( $(const char* name_), H5F_ACC_RDWR, H5P_DEFAULT )}|]



-- * Dataset creation

-- herr_t H5LTmake_dataset_double ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const double *buffer )
-- Purpose:
--     Creates and writes a dataset.  
-- Description:
--     H5LTmake_dataset creates and writes a dataset named dset_name attached to the object specified by the identifier loc_id.
--     The dataset’s datatype will be native floating-point double, H5T_NATIVE_DOUBLE. 
-- Parameters:
-- hid_t loc_id
--     IN: Identifier of the file or group to create the dataset within. 
-- const char *dset_name
--     IN: The name of the dataset to create. 
-- int rank
--     IN: Number of dimensions of dataspace. 
-- const hsize_t * dims
--     IN: An array of the size of each dimension. 
-- const double * buffer
--     IN: Buffer with data to be written to the dataset.
-- Returns:
--     Returns a non-negative value if successful; otherwise returns a negative value.
makeDatasetDouble loc dsname dims buffer =
  let
    rank = fromIntegral $ length dims
  in 
    withCString dsname $ \dsname_ ->
    withArray dims $ \dims_ -> 
    VS.unsafeWith buffer $ \ bp ->
        [C.exp| herr_t{
        H5LTmake_dataset_double( $(hid_t loc), $(const char* dsname_), $(int rank), $(const hsize_t* dims_), $(double* bp) )
        } |]  





-- * HDF5 image operations
-- The HDF5 Image API defines a standard storage for HDF5 datasets that are intended to be interpreted as images. The specification for this API is presented in another document: HDF5 Image and Palette Specification. This version of the API is primarily concerned with two dimensional raster data similar to HDF4 Raster Images. The HDF5 image API uses the Lite HDF5 API. 

-- herr_t H5IMread_image ( hid_t loc_id, const char *dset_name, unsigned char *buffer ) 
-- Purpose:
--     Reads image data from disk. 
-- Description:
--     H5IMread_image reads a dataset named dset_name attached to the file or group specified by the identifier loc_id. 
-- Parameters:
--     hid_t loc_id 	IN: Identifier of the file or group to read the dataset from.
--     const char *dset_name 	IN: The name of the dataset.
--     unsigned char *buffer     	OUT: Buffer with data to store the image.
-- Returns:
--     Returns a non-negative value if successful; otherwise returns a negative value.

readImage locId name = undefined




-- | Helpers

class Sized v where
  type SizedData v :: * 
  dat :: v -> SizedData v
  size :: v -> [Hsize]
  rank :: v -> CInt

-- data Sized v = Sized { szDat :: v, szDims :: [Hsize] } deriving (Eq, Show)
