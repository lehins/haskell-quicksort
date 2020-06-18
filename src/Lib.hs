{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Lib
    ( quicksortVector
    , quicksortAlgorithms
    , quicksortPrimArray
    , quicksortC
    , module Slow
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Int
import Data.Massiv.Array as A
import Data.Primitive.PrimArray
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign.C.Types
import Foreign.Ptr
import System.IO.Unsafe

import Slow

quicksortVector :: (V.Vector v a, Ord a) => v a -> v a
quicksortVector = V.modify go
  where
    go !xs =
      unless (VM.length xs < 2) $ do
        p <- VM.unsafeRead xs (VM.length xs `div` 2)
        j <- VM.unstablePartition (< p) xs
        let (l, pr) = VM.splitAt j xs
        k <- VM.unstablePartition (== p) pr
        go l
        go $ VM.drop k pr
{-# INLINE quicksortVector #-}

quicksortAlgorithms :: (V.Vector v a, Ord a) => v a -> v a
quicksortAlgorithms v =
  runST $ do
    mv <- V.thaw v
    Intro.sort mv
    V.unsafeFreeze mv
{-# INLINE quicksortAlgorithms #-}


quicksortPrimArray :: PrimArray Int64 -> PrimArray Int64
quicksortPrimArray arr = runST $ do
  let sz = sizeofPrimArray arr
  marr <- newPrimArray sz
  copyPrimArray marr 0 arr 0 sz
  quicksortMutablePrimArray marr
  unsafeFreezePrimArray marr
{-# INLINE quicksortPrimArray #-}

quicksortMutablePrimArray ::
     (PrimMonad m) => MutablePrimArray (PrimState m) Int64 -> m ()
quicksortMutablePrimArray marr
  | sz < 2 = pure ()
  | otherwise = qsort 0 (sz - 1)
  where
    sz = sizeofMutablePrimArray marr
    getLo !l !h !p = do
      x <- readPrimArray marr l
      if l < h && x <= p
        then getLo (l + 1) h p
        else pure l
    getHi !l !h !p = do
      x <- readPrimArray marr h
      if h > l && x >= p
        then getHi l (h - 1) p
        else pure h
    go !l !h !p = do
      l' <- getLo l h p
      h' <- getHi l' h p
      if l' < h'
        then do
          t <- readPrimArray marr l'
          writePrimArray marr l' =<< readPrimArray marr h'
          writePrimArray marr h' t
          go l' h' p
        else pure l'
    qsort !lo !hi
      | lo < hi = do
        p <- readPrimArray marr hi
        l <- go lo hi p
        writePrimArray marr hi =<< readPrimArray marr l
        writePrimArray marr l p
        qsort lo (l - 1)
        qsort (l + 1) hi
      | otherwise = pure ()
{-# INLINE quicksortMutablePrimArray #-}

-- quicksortArrayS ::
--      (Mutable r Ix1 e, Ord e) => Array r Ix1 e -> Array r Ix1 e
-- quicksortArrayS arr = withMArrayST arr quicksortMArrayS
-- {-# INLINE quicksortArrayS #-}

-- quicksortMArrayS ::
--      (Ord e, Mutable r Ix1 e, PrimMonad m) => A.MArray (PrimState m) r Ix1 e -> m ()
-- quicksortMArrayS marr = qsort 0 (unSz (msize marr) - 1)
--   where
--     leSwap i j = do
--       ei <- A.unsafeRead marr i
--       ej <- A.unsafeRead marr j
--       if ei < ej
--         then do
--           A.unsafeWrite marr i ej
--           A.unsafeWrite marr j ei
--           pure ei
--         else pure ej
--     {-# INLINE leSwap #-}
--     getPivot lo hi = do
--       let !mid = (hi + lo) `div` 2
--       _ <- leSwap mid lo
--       _ <- leSwap hi lo
--       leSwap mid hi
--     {-# INLINE getPivot #-}
--     qsort !lo !hi =
--       when (lo < hi) $ do
--         p <- getPivot lo hi
--         l <- unstablePartitionRegionM marr (< p) lo (hi - 1)
--         h <- unstablePartitionRegionM marr (== p) l hi
--         qsort lo (l - 1)
--         qsort h hi
-- {-# INLINE quicksortMArrayS #-}

-- unstablePartitionRegionM ::
--      forall r e m. (Mutable r Ix1 e, PrimMonad m)
--   => MArray (PrimState m) r Ix1 e
--   -> (e -> Bool)
--   -> Ix1 -- ^ Start index of the region
--   -> Ix1 -- ^ End index of the region
--   -> m Ix1
-- unstablePartitionRegionM marr f start end = fromLeft start (end + 1)
--   where
--     fromLeft i j
--       | i == j = pure i
--       | otherwise = do
--         x <- A.unsafeRead marr i
--         if f x
--           then fromLeft (i + 1) j
--           else fromRight i (j - 1)
--     fromRight i j
--       | i == j = pure i
--       | otherwise = do
--         x <- A.unsafeRead marr j
--         if f x
--           then do
--             A.unsafeWrite marr j =<< A.unsafeRead marr i
--             A.unsafeWrite marr i x
--             fromLeft (i + 1) j
--           else fromRight i (j - 1)
-- {-# INLINE unstablePartitionRegionM #-}

-- quicksortArray ::
--      (Mutable r Ix1 e, Ord e) => Array r Ix1 e -> Array r Ix1 e
-- quicksortArray arr =
--   unsafePerformIO $
--   withMArray
--     arr
--     (\n s ->
--        quicksortMArray (trivialScheduler_ {numWorkers = n, scheduleWork = s}))
-- {-# INLINE quicksortArray #-}

-- quicksortMArray ::
--      (Ord e, Mutable r Ix1 e, PrimMonad m)
--   => Scheduler m ()
--   -> A.MArray (PrimState m) r Ix1 e
--   -> m ()
-- quicksortMArray scheduler marr =
--   scheduleWork scheduler $ qsort (numWorkers scheduler) 0 (unSz (msize marr) - 1)
--   where
--     leSwap i j = do
--       ei <- A.unsafeRead marr i
--       ej <- A.unsafeRead marr j
--       if ei < ej
--         then do
--           A.unsafeWrite marr i ej
--           A.unsafeWrite marr j ei
--           pure ei
--         else pure ej
--     {-# INLINE leSwap #-}
--     getPivot lo hi = do
--       let !mid = (hi + lo) `div` 2
--       _ <- leSwap mid lo
--       _ <- leSwap hi lo
--       leSwap mid hi
--     {-# INLINE getPivot #-}
--     qsort !n !lo !hi =
--       when (lo < hi) $ do
--         p <- getPivot lo hi
--         l <- unstablePartitionRegionM marr (< p) lo hi
--         h <- unstablePartitionRegionM marr (== p) l hi
--         if n > 0
--           then do
--             let !n' = n - 1
--             scheduleWork scheduler $ qsort n' lo (l - 1)
--             scheduleWork scheduler $ qsort n' h hi
--           else do
--             qsort n lo (l - 1)
--             qsort n h hi
-- {-# INLINE quicksortMArray #-}


quicksortC :: VS.Vector CLong -> VS.Vector CLong
quicksortC v = unsafePerformIO $ do
  mv <- VS.thaw v
  VSM.unsafeWith mv (\ptr -> c_qsort ptr 0 (fromIntegral (VS.length v - 1)))
  VS.unsafeFreeze mv
{-# INLINE quicksortC #-}


foreign import ccall unsafe "qsort.c qsort"
  c_qsort :: Ptr CLong -> CLong -> CLong -> IO ()
