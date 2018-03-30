module Data.HDF5.Store where

import qualified Data.Vector.Storable as VS
import Foreign.C.Types (CInt, CChar, CDouble, CFloat, CSize)
import Foreign.Storable
import Control.Exception

import Data.HDF5.Internal.Types (Herr, Hid, Hsize, H5T_class_t)

class Storable a => StorableDataset a where
  -- | Allocate and write a dataset
  makeDataset :: Hid  -- ^ Identifier
    -> String    -- ^ Dataset name
    -> [Hsize]   -- ^ Dimensions
    -> VS.Vector a -- ^ Dataset contents
    -> IO ()
  -- | Read a dataset
  readDataset :: Hid -- ^ Identifier
    -> String        -- ^ Dataset name
    -> IO (VS.Vector a)




data Dataset a = DS {
    dsSize :: [Hsize]
  , dsData :: VS.Vector a
                    } deriving (Eq, Show)

class Storable a => StorableDS a where
  makeDS :: Hid -> String -> Dataset a -> IO ()
  readDS :: Hid -> String -> IO (Dataset a)






-- from https://github.com/ian-ross/hnetcdf/blob/master/Data/NetCDF/Store.hs

-- -- | Class representing containers suitable for storing values read
-- -- from NetCDF variables.  Just has methods to convert back and forth
-- -- between the store and a foreign pointer, and to perform simple
-- -- mapping over the store.  The NcStoreExtraCon associated
-- -- constraint-kinded type is used to track extra class constraints on
-- -- elements needed for some store types.
-- class NcStore s where
--   type NcStoreExtraCon s a :: Constraint
--   type NcStoreExtraCon s a = ()
--   toForeignPtr :: (Storable e, NcStoreExtraCon s e) =>
--                   s e -> ForeignPtr e
--   fromForeignPtr :: (Storable e, NcStoreExtraCon s e) =>
--                     ForeignPtr e -> [Int] -> s e
--   smap :: (Storable a, Storable b, NcStoreExtraCon s a, NcStoreExtraCon s b) =>
-- (a -> b) -> s a -> s b
