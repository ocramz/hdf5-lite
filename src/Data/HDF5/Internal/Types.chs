{-# language CPP #-}
module Data.HDF5.Internal.Types where

#include <H5Tpublic.h>
#include <hdf5_hl.h>

-- | HDF5 integer error codes
type Herr = {#type herr_t#} 

-- | HDF Type of atoms to return to users
type Hid = {#type hid_t#} 

-- | HDF5 file object sizes (64 bits by default i.e. an unsigned CLLong)
type Hsize = {#type hsize_t #} 

-- | HDF5 "ternary" Boolean type. Functions that return `htri_t' return zero (false), positive (true), or negative (failure).
type Htri = {#type htri_t #}



-- | HDF5 native datatypes : https://support.hdfgroup.org/HDF5/doc/RM/PredefDTypes.html
  
-- type HNativeDouble = {#type H5T_NATIVE_DOUBLE #}


-- | flags

