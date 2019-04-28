{-# LANGUAGE BangPatterns #-}
--{-# LANGUAGE TypeApplications #-}
module Main where

import Control.DeepSeq
import Criterion.Main
import Data.Coerce
import Data.Massiv.Array as A
import Data.Primitive.PrimArray
-- import Data.Array.MArray as MA
-- import Data.Array.IO
import Data.Array.Unboxed (UArray)
import Data.Int
import qualified Data.Vector.Storable as V
import Foreign.C.Types (CLong(..))
import qualified GHC.Exts as IsList (fromList)
import Lib
import Prelude as P
import System.Random

rs :: Int -> [Int64]
rs n = take n $ randoms (mkStdGen n)

instance NFData (UArray i a) where
  rnf v = v `seq` ()

instance NFData (PrimArray a) where
  rnf v = v `seq` ()

main :: IO ()
main = do
  let k = 1000000
      !xsRandom = rs k
      !xsSorted = fromIntegral <$> [1 .. k]
      !xsReversed = reverse xsSorted
      !xsReplicated = P.replicate k 31415
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
    [ mkGroup "random" xsRandom
    , mkGroup "sorted" xsSorted
    , mkGroup "reversed sorted" xsReversed
    , mkGroup "replicated" xsReplicated
    ]

mkGroup :: String -> [Int64] -> Benchmark
mkGroup name xs =
  bgroup
    name
    [ env (pure (A.fromList Seq xs :: Array S Ix1 Int64)) $ \v ->
        bench "Array Seq" $ nf quicksortArrayS v
    , env (pure (A.fromList Seq xs :: Array S Ix1 Int64)) $ \v ->
        bench "Array" $ nf quicksortArray v
    , env (pure (A.fromList Par xs :: Array S Ix1 Int64)) $ \v ->
        bench "Array Par" $ nf quicksortArray v
    , env (pure $ V.fromList xs) $ \v ->
        bench "Vector Algorithms" $ nf quicksortAlgorithms v
    ]
