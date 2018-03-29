{-# language OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Data.HDF5.Lite.Internal where

import           Data.Monoid ((<>), mempty)
import qualified Data.Map as M

import           Foreign.C.Types
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable(..))
import qualified Language.Haskell.TH as TH

import           Language.C.Inline as C
import           Language.C.Inline.Context
import qualified Language.C.Types as C

import Data.HDF5.Internal.Types


C.include "<hdf5_hl.h>"



-- | inline-c context
hdf5ctx :: Context
hdf5ctx = baseCtx <> funCtx <> vecCtx <> bsCtx <> ctx
  where
    ctx = mempty { ctxTypesTable = hdf5TypesTable }

-- | HDF5 types table for inline-c
hdf5TypesTable :: M.Map C.TypeSpecifier TH.TypeQ
hdf5TypesTable = M.fromList [
    (C.TypeName "herr_t", [t| Herr |])
  , (C.TypeName "hid_t", [t| Hid |])
  , (C.TypeName "hsize_t", [t| Hsize |])
                            ]



{-
-- * Enums

type Nag_Boolean = CInt
type Nag_ErrorControl = CInt

-- * Utils

type Nag_Integer = CLong

-- * Context

nagCtx :: Context
nagCtx = baseCtx <> funCtx <> vecCtx <> ctx
  where
    ctx = mempty
      { ctxTypesTable = nagTypesTable
      }

nagTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
nagTypesTable = Map.fromList
  [ -- TODO this might not be a long, see nag_types.h
    (C.TypeName "Integer", [t| Nag_Integer |])
-}  
