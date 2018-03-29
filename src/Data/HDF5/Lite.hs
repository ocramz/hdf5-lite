{-# language OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
{-# language TypeFamilies #-}
module Data.HDF5.Lite where

import           Data.Functor ((<$>))

import Control.Monad.Catch (throwM, MonadThrow(..))
import Control.Exception

import           Foreign.C.String (peekCString, withCString)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import           Foreign.Ptr (Ptr)
import Foreign.Storable

-- import GHC.Prim

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VM

import           Data.HDF5.Internal.Types (Herr, Hid, Hsize)
import qualified Data.HDF5.Lite.Internal as H
import qualified Language.C.Inline as C

C.context H.hdf5ctx

C.include "<hdf5_hl.h>"


  

-- | HDF5 Lite API reference: https://support.hdfgroup.org/HDF5/doc/HL/RM_H5LT.html



-- -- herr_t H5LTmake_dataset ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, hid_t type_id, const void *buffer )
-- -- Purpose:
-- --     Creates and writes a dataset of a type type_id.      
-- -- Description:
-- --     H5LTmake_dataset creates and writes a dataset named dset_name attached to the object specified by the identifier loc_id.
-- --     The parameter type_id can be any valid HDF5 predefined native datatype; For example, setting type_id to H5T_NATIVE_INT will result in a dataset of signed integer datatype.
-- -- Parameters:
-- --     hid_t loc_id 	IN: Identifier of the file or group to create the dataset within.
-- --     const char *dset_name    	IN: The name of the dataset to create.
-- --     int rank 	IN: Number of dimensions of dataspace.
-- --     const hsize_t * dims 	IN: An array of the size of each dimension.
-- --     hid_t type_id 	IN: Identifier of the datatype to use when creating the dataset.
-- --     const void * buffer 	IN: Buffer with data to be written to the dataset.
-- -- Returns:
-- --     Returns a non-negative value if successful; otherwise returns a negative value.

-- -- makeDataset ::
-- --   Hid -> Ptr CChar -> CInt -> Ptr Hsize -> Hid -> VM.IOVector CDouble -> IO Herr
-- makeDatasetDouble
--   :: Hid -> String -> [Hsize] -> VS.Vector CDouble -> IO Herr
-- makeDatasetDouble loc dsname dims buffer =
--   let
--     rank = fromIntegral $ length dims
--   in 
--     withCString dsname $ \dsname_ ->
--     withArray dims $ \dims_ -> 
--     VS.unsafeWith buffer $ \ bp -> 
--     [C.exp| herr_t{
--         H5LTmake_dataset( $(hid_t loc), $(const char* dsname_), $(int rank), $(const hsize_t* dims_), H5T_NATIVE_DOUBLE, $(double* bp) )
--         } |]  



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





-- | Helpers






class Sized v where
  type SizedData v :: * 
  dat :: v -> SizedData v
  size :: v -> [Hsize]
  rank :: v -> CInt

-- data Sized v = Sized { szDat :: v, szDims :: [Hsize] } deriving (Eq, Show)
