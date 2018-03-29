{-# language CPP #-}
module Data.HDF5.Internal.Types where

#include <hdf5_hl.h>

newtype HidT = HidT {#type hid_t#} deriving (Eq, Show)

  
