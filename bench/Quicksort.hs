{-# LANGUAGE BangPatterns #-}
--{-# LANGUAGE TypeApplications #-}
module Main where

import Control.DeepSeq
import Criterion.Main
import Data.Coerce
import Data.Primitive.PrimArray
import Data.Massiv.Array as A
-- import Data.Array.MArray as MA
-- import Data.Array.IO
import Data.Array.Unboxed (UArray)
import qualified Data.Vector.Storable as V
import Foreign.C.Types (CLong(..))
import Lib
import Prelude as P
import System.Random
import Data.Int
import qualified GHC.Exts as IsList (fromList)

rs :: Int -> [Int64]
rs n = take n $ randoms (mkStdGen n)

instance NFData (UArray i a) where
  rnf v = v `seq` ()

instance NFData (PrimArray a) where
  rnf v = v `seq` ()

main :: IO ()
main = do
  let k = 1000000
      !xs = rs k
  defaultMain
    [ bgroup
        "QuickSort"
        [ -- env (pure xs) $ \ls -> bench "List" $ nf quicksortList ls
          --env (pure xs) $ \ls -> bench "List Par" $ nf quicksortListPar ls
          -- , env (MA.listArray @IOUArray (0, k) (fmap fromIntegral xs) >>= MA.freeze) $ \v ->
          --     bench "UArray" $ whnf quicksortUArray v
          -- env (pure $ IsList.fromList xs) $ \v ->
          --   bench "PrimArray" $ whnf quicksortPrimArray v
         env (pure $ V.fromList xs) $ \v ->
            bench "Vector Algorithms" $ nf quicksortAlgorithms v
        , env (pure $ V.fromList xs) $ \v ->
            bench "Vector" $ nf quicksortVector v
        , env (pure $ V.fromList (coerce xs)) $ \v ->
            bench "C" $ nf quicksortC v
        , env (pure (A.fromList Seq xs :: Array S Ix1 Int64)) $ \v ->
            bench "Array" $ nf quicksortArray v
        , env (pure (A.fromList Par xs :: Array S Ix1 Int64)) $ \v ->
            bench "Array Par" $ nf quicksortArray v
        ]
    ]
