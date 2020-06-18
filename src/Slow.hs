{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Slow
    ( quicksortList
    , quicksortListPar
    , pQuicksort
    , quicksortUArray
    ) where

import Control.Monad
import Data.Array.IO as MA
import Data.Array.Unboxed as MA (UArray, bounds)
import qualified Data.Array.Unsafe as MA
import Data.IORef
import Control.Parallel
import Data.List
import System.IO.Unsafe

quicksortList :: Ord a => [a] -> [a]
quicksortList = go
  where
    go [] = []
    go (p:xs) =
      let lesser = filter (< p) xs
          greater = filter (>= p) xs
      in go lesser ++ [p] ++ go greater
{-# INLINE quicksortList #-}


quicksortListPar :: Ord a => [a] -> [a]
quicksortListPar xs = pQuicksort 8 xs -- 8 is the number of sparks used to sort
{-# INLINE quicksortListPar #-}

-- pQuicksort, parallelQuicksort
-- As long as n > 0 evaluates the lower and upper part of the list in parallel,
-- when we have recursed deep enough, n==0, this turns into a serial quicksort.
pQuicksort :: Ord a => Int -> [a] -> [a]
pQuicksort _ [] = []
pQuicksort 0 (x:xs) =
  let (lower, upper) = partition (< x) xs
  in pQuicksort 0 lower ++ [x] ++ pQuicksort 0 upper
pQuicksort n (x:xs) =
  let (lower, upper) = partition (< x) xs
      l = pQuicksort (n `div` 2) lower
      u = [x] ++ pQuicksort (n `div` 2) upper
  in (par u l) ++ u
{-# INLINE pQuicksort #-}


quicksortUArray :: UArray Int Int -> UArray Int Int
quicksortUArray arr = unsafePerformIO $ do
  marr <- MA.thaw arr
  quicksortIOUArray marr 0 (MA.rangeSize (MA.bounds arr) - 1)
  MA.unsafeFreeze marr
{-# INLINE quicksortUArray #-}

quicksortIOUArray :: IOUArray Int Int -> Int -> Int -> IO ()
quicksortIOUArray a lo hi = do
  let z :: IO (IORef Int)
      z = newIORef 0
      (.=) = writeIORef
      ref .=. action = do v <- action; ref .= v
      (!) = readArray
      (.!) a' ref = MA.readArray a' =<< get ref
      get = readIORef
      (.<) = liftM2 (<)
      (.>) = liftM2 (>)
      (.<=) = liftM2 (<=)
      (.>=) = liftM2 (>=)
      (.&&) = liftM2 (&&)
  (h,l,p,t) <- liftM4 (,,,) z z z z

  when (lo < hi) $ do
    l .= lo
    h .= hi
    p .=. (a!hi)

    doWhile (get l .< get h) $ do
      while ((get l .< get h) .&& ((a.!l) .<= get p)) $ do
        modifyIORef l succ
      while ((get h .> get l) .&& ((a.!h) .>= get p)) $ do
        modifyIORef h pred
      b <- get l .< get h
      when b $ do
        t .=. (a.!l)
        lVal <- get l
        hVal <- get h
        MA.writeArray a lVal =<< a!hVal
        MA.writeArray a hVal =<< get t

    lVal <- get l
    MA.writeArray a hi =<< a!lVal
    MA.writeArray a lVal =<< get p

    hi' <- fmap pred (get l)
    quicksortIOUArray a lo hi'
    lo' <- fmap succ (get l)
    quicksortIOUArray a lo' hi

  where doWhile cond foo = do
          _ <- foo
          b <- cond
          when b $ doWhile cond foo
        while cond foo = do
          b <- cond
          when b $ foo >> while cond foo
{-# INLINE quicksortIOUArray #-}
