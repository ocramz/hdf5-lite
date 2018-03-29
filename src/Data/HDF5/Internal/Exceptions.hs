{-# language DeriveGeneric #-}
module Data.HDF5.Internal.Exceptions where

import GHC.Generics

import Control.Monad
import Control.Monad.Catch

import Data.HDF5.Internal.Types (Herr)

data HDF5Exception = HDF5Exception Herr deriving (Eq, Show, Generic)
instance Exception HDF5Exception


throwH :: MonadThrow m => m Herr -> m ()
throwH io = do
  ec <- io
  when (ec < 0) $ throwM (HDF5Exception ec)


