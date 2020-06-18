{-# LANGUAGE BangPatterns #-}
--{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Concurrent
import Control.DeepSeq
import Criterion.Main
import Data.Coerce
import Data.Massiv.Array as A
import Data.Primitive.PrimArray
import qualified Data.Primitive.Sort as PA
-- import Data.Array.MArray as MA
-- import Data.Array.IO
import Data.Array.Unboxed (UArray)
import Data.Int
import qualified Data.Vector.Primitive as V
import Foreign.C.Types (CLong(..))
import qualified GHC.Exts as IsList (fromList)
import Lib
import Map
import Prelude as P
import System.Random

rs :: Int -> [Int64]
rs n = P.take n $ randoms (mkStdGen n)

instance NFData (UArray i a) where
  rnf v = v `seq` ()

instance NFData (PrimArray a) where
  rnf v = v `seq` ()

main :: IO ()
main = do
  let k = 1000000
      !xsRandom = rs k
      !xsSorted = fromIntegral <$> [1 .. k]
      !xsReversed = P.reverse xsSorted
      !xsReplicated = P.replicate k 31415
  n <- getNumCapabilities
  defaultMain
          -- env (pure xs) $ \ls -> bench "List" $ nf quicksortList ls
          --env (pure xs) $ \ls -> bench "List Par" $ nf quicksortListPar ls
          -- , env (MA.listArray @IOUArray (0, k) (fmap fromIntegral xs) >>= MA.freeze) $ \v ->
          --     bench "UArray" $ whnf quicksortUArray v
          -- env (pure $ IsList.fromList xs) $ \v ->
          --   bench "PrimArray" $ whnf quicksortPrimArray v
        --  env (pure $ V.fromList xs) $ \v ->
        --     bench "Vector Algorithms" $ nf quicksortAlgorithms v
        -- , env (pure $ V.fromList xs) $ \v ->
        --     bench "Vector" $ nf quicksortVector v
        -- , env (pure $ V.fromList (coerce xs)) $ \v ->
        --     bench "C" $ nf quicksortC v
        -- , env (pure (A.fromList Par xs :: Array S Ix1 Int64)) $ \v ->
        --     bench "Array Par" $ nf quicksortArray v
    [ bgroup
        "List" -- no matter of a solution get a list sorted
        [ mkGroupList n "random" xsRandom
        , mkGroupList n "sorted" xsSorted
        , mkGroupList n "reversed sorted" xsReversed
        , mkGroupList n "replicated" xsReplicated
        ]
    , bgroup
        "Array" -- Use a more appropriate data structure
        [ mkGroup n "random" xsRandom
        , mkGroup n "sorted" xsSorted
        , mkGroup n "reversed sorted" xsReversed
        , mkGroup n "replicated" xsReplicated
        ]
    ]

mkGroupList :: Int -> String -> [Int64] -> Benchmark
mkGroupList n name xs =
  bgroup
    name
    [ env (pure xs) $ \v ->
        bench "Array Seq" $
        nf
          (A.toList .
           A.quicksort . (A.fromList Seq :: [Int64] -> Array S Ix1 Int64))
          v
    , env (pure xs) $ \v ->
        bench "Array Par" $
        nf
          (A.toList .
           A.quicksort . (A.fromList Par :: [Int64] -> Array S Ix1 Int64))
          v
    , env (pure xs) $ \v ->
        bench "Vector Algorithms" $
        nf (V.toList . quicksortAlgorithms . V.fromList) v
    , env (pure xs) $ \v -> bench "Map Seq" $ nf ssort v
    , env (pure xs) $ \v -> bench "Map Par" $ nf (pSort n) v
    ]


mkGroup :: Int -> String -> [Int64] -> Benchmark
mkGroup n name xs =
  bgroup
    name
    [ env (pure (A.fromList Seq xs :: Array S Ix1 Int64)) $ \v ->
        bench "massiv/Array Seq (quicksort)" $ nf A.quicksort v
    , env (pure (A.fromList Par xs :: Array S Ix1 Int64)) $ \v ->
        bench "massiv/Array Par (quicksort)" $ nf A.quicksort v
    , env (pure $ primArrayFromList xs) $ \v ->
        bench "primitive-sort/PrimArray Par (insertion sort)" $ nf PA.sort v
    , env (pure $ V.fromList xs) $ \v ->
        bench "vector-algorithms/Vector (intro sort)" $ nf quicksortAlgorithms v
    ]
