{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module LibSpec (spec) where

import Common
import Data.Coerce
import Data.Int
import Data.List
import Data.Massiv.Array as A
import qualified Data.Vector.Storable as V
import Foreign.C.Types (CLong(..))
import Lib
import Data.Array.Unboxed (UArray)
import Data.Array.IArray

prop_IsSorted :: (b -> b) -> ([Int64] -> b) -> (b -> [Int64]) -> [Int64] -> Property
prop_IsSorted sortWith from to xs =
  to (sortWith (from xs)) === sort xs

spec :: Spec
spec =
  describe "QuickSort" $ do
    it "List" $ property $ prop_IsSorted quicksortList id id
    it "List Par" $ property $ prop_IsSorted quicksortListPar id id
    it "Vector" $ property $ prop_IsSorted quicksortVector V.fromList V.toList
    it "VectorAlgorithms" $ property $ prop_IsSorted quicksortAlgorithms V.fromList V.toList
    it "Array" $ property $ prop_IsSorted (quicksortArrayS @P) (A.fromList Seq) A.toList
    it "Array Par" $ property $ prop_IsSorted (quicksortArray @P) (A.fromList (ParN 4)) A.toList
    it "C" $ property $ prop_IsSorted quicksortC (V.fromList . coerce) (V.toList . coerce)
    it "UArray" $ property $ prop_IsSorted quicksortUArray toUArray (fmap fromIntegral . elems)
  where toUArray xs =
          listArray @UArray (0, length xs - 1) (fmap fromIntegral xs)
