module Data.HDF5.Store where






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
