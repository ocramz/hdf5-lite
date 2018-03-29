{-# language CPP #-}
module Data.HDF5.Internal.Types where

#include <hdf5_hl.h>

-- | HDF5 integer error codes
newtype Herr = Herr {#type herr_t#} deriving (Eq, Show)

-- | HDF Type of atoms to return to users
newtype Hid = Hid {#type hid_t#} deriving (Eq, Show)

-- | HDF5 file object sizes (64 bits by default i.e. an unsigned CLLong)
newtype Hsize = Hsize {#type hsize_t #} deriving (Eq, Show)



  
