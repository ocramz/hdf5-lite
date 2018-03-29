{-# language OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Data.HDF5.Lite where

import           Data.Functor ((<$>))
import           Foreign.C.String (peekCString)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr)

import           Data.HDF5.Internal.Types (Herr, Hid, Hsize)
import qualified Data.HDF5.Lite.Internal as H
import qualified Language.C.Inline as C

C.context H.hdf5ctx

C.include "<hdf5_hl.h>"

-- herr_t H5LTmake_dataset ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, hid_t type_id, const void *buffer ) 
makeDataset loc dsname rank dims tyid buffer =
  [C.exp| herr_t{
      H5LTmake_dataset( $(Hid loc), $(const char* dsname), $(int rank), $(const Hsize* dims), $(Hid tyid), $(const void* buffer)  )
      } |]





