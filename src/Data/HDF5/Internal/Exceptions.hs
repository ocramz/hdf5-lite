{-# language DeriveGeneric #-}
module Data.HDF5.Internal.Exceptions where

import GHC.Generics

import Control.Monad
import Control.Monad.Catch

import Data.HDF5.Internal.Types (Herr, Htri)

data HDF5Exception = HDF5Exception Herr deriving (Eq, Show, Generic)
instance Exception HDF5Exception


throwH :: MonadThrow m => m Herr -> m ()
throwH io = do
  ec <- io
  when (ec < 0) $ throwM (HDF5Exception ec)


throwTri :: MonadThrow m => m Htri -> m Bool
throwTri io = do
  ec <- io
  if
    ec > 0
    then return True
    else if ec == 0 then return False else throwM (HDF5Exception ec)
