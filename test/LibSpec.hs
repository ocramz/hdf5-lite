module Main where

-- import Test.Hspec
-- import Test.Hspec.QuickCheck
import Foreign.C.Types (CInt, CChar, CDouble, CFloat, CSize)
import qualified Data.Vector.Storable as VS

import Data.HDF5.Lite 


-- main :: IO ()
-- main = hspec spec

-- spec :: Spec
-- spec =
--   describe "Lib" $ do
--     it "works" $ do
--       True `shouldBe` True
--     -- prop "ourAdd is commutative" $ \x y ->
--     --   ourAdd x y `shouldBe` ourAdd y x



main = test1

test1 = withFileCreate "moo.h5" $ \fid ->
  makeDataset fid "/dset" [2,2] (VS.fromList ([1..4] :: [CDouble]))

