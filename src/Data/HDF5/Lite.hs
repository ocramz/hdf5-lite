{-# language OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Data.HDF5.Lite where

import           Data.Functor ((<$>))
import           Foreign.C.String (peekCString, withCString)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr)
import Foreign.Storable

import           Data.HDF5.Internal.Types (Herr, Hid, Hsize)
import qualified Data.HDF5.Lite.Internal as H
import qualified Language.C.Inline as C

C.context H.hdf5ctx

C.include "<hdf5_hl.h>"

-- herr_t H5LTmake_dataset ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, hid_t type_id, const void *buffer )
-- makeDataset
--   :: Hid -> String -> CInt -> Ptr Hsize -> Hid -> Ptr () -> IO Herr
makeDataset loc dsname rank dims tyid buffer = withCString dsname $ \dsname_ -> 
  [C.exp| herr_t{
      H5LTmake_dataset( $(hid_t loc), $(const char* dsname_), $(int rank), $(const hsize_t* dims), $(hid_t tyid), $(const void* buffer) )
      } |]





