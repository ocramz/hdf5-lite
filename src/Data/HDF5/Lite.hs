{-# language OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Data.HDF5.Lite where

import           Data.Functor ((<$>))
import           Foreign.C.String (peekCString, withCString)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import           Foreign.Ptr (Ptr)
import Foreign.Storable

import           Data.HDF5.Internal.Types (Herr, Hid, Hsize)
import qualified Data.HDF5.Lite.Internal as H
import qualified Language.C.Inline as C

C.context H.hdf5ctx

C.include "<hdf5_hl.h>"

-- herr_t H5LTmake_dataset ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, hid_t type_id, const void *buffer )
-- Purpose:
--     Creates and writes a dataset of a type type_id.      
-- Description:
--     H5LTmake_dataset creates and writes a dataset named dset_name attached to the object specified by the identifier loc_id.

--     The parameter type_id can be any valid HDF5 predefined native datatype; For example, setting type_id to H5T_NATIVE_INT will result in a dataset of signed integer datatype.
-- Parameters:
--     hid_t loc_id 	IN: Identifier of the file or group to create the dataset within.
--     const char *dset_name    	IN: The name of the dataset to create.
--     int rank 	IN: Number of dimensions of dataspace.
--     const hsize_t * dims 	IN: An array of the size of each dimension.
--     hid_t type_id 	IN: Identifier of the datatype to use when creating the dataset.
--     const void * buffer 	IN: Buffer with data to be written to the dataset.
-- Returns:
--     Returns a non-negative value if successful; otherwise returns a negative value. 
makeDataset
  :: Hid -> String -> CInt -> [Hsize] -> Hid -> Ptr () -> IO Herr
makeDataset loc dsname rank dims tyid buffer =
  withCString dsname $ \dsname_ ->
  withArray dims $ \dims_ -> 
  [C.exp| herr_t{
      H5LTmake_dataset( $(hid_t loc), $(const char* dsname_), $(int rank), $(const hsize_t* dims_), $(hid_t tyid), $(const void* buffer) )
      } |]





