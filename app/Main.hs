{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Lib
import Data.Int
import Data.Vector.Primitive

main :: IO ()
main = do
  let x = quicksortVector @Vector [1 .. 10 :: Int64]
  x `seq` pure ()
