{-# language CPP #-}
module Data.HDF5.Internal.Types where

#include <hdf5_hl.h>

-- | HDF5 integer error codes
type Herr = {#type herr_t#} 

-- | HDF Type of atoms to return to users
type Hid = {#type hid_t#} 

-- | HDF5 file object sizes (64 bits by default i.e. an unsigned CLLong)
type Hsize = {#type hsize_t #} 



  
