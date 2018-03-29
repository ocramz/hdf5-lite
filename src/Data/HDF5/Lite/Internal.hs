{-# language OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Data.HDF5.Lite.Internal where

import           Data.Monoid ((<>), mempty)
import           Foreign.C.Types
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable(..))
import qualified Language.Haskell.TH as TH

import           Language.C.Inline
import           Language.C.Inline.Context
import qualified Language.C.Types as C


#include <hdf5_hl.h.h>
