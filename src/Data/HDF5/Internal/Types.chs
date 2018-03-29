{-# language CPP, DeriveAnyClass #-}
module Data.HDF5.Internal.Types where

import Foreign.Storable

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



-- /* These are the various classes of datatypes */
-- /* If this goes over 16 types (0-15), the file format will need to change) */
-- typedef enum H5T_class_t {
--     H5T_NO_CLASS         = -1,  /*error                                      */
--     H5T_INTEGER          = 0,   /*integer types                              */
--     H5T_FLOAT            = 1,   /*floating-point types                       */
--     H5T_TIME             = 2,   /*date and time types                        */
--     H5T_STRING           = 3,   /*character string types                     */
--     H5T_BITFIELD         = 4,   /*bit field types                            */
--     H5T_OPAQUE           = 5,   /*opaque types                               */
--     H5T_COMPOUND         = 6,   /*compound types                             */
--     H5T_REFERENCE        = 7,   /*reference types                            */
--     H5T_ENUM		 = 8,	/*enumeration types                          */
--     H5T_VLEN		 = 9,	/*Variable-Length types                      */
--     H5T_ARRAY	         = 10,	/*Array types                                */

--     H5T_NCLASSES                /*this must be last                          */
-- } H5T_class_t;

{#enum H5T_class_t {underscoreToCase} deriving (Eq, Storable) #}



-- | flags

