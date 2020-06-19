{-# LANGUAGE BangPatterns, TupleSections #-}
module Map where

import qualified Data.Map.Strict as M
import Data.Map (Map)
import Data.List
import Control.Parallel.Strategies
import Data.Map.Internal (Map (..), splitLookup, link)

type Bag a = Map a Int

ssort :: Ord a => [a] -> [a]
ssort xs =
  let m = M.fromListWith (+) $ (,1) <$> xs
  in concat [replicate c x | (x,c) <- M.toList m]
{-# INLINE ssort #-}


-- | Every Nth element, including the first
everyNth :: Int -> [a] -> [a]
everyNth n | n <= 0 = error "What you doing?"
everyNth n = go 0 where
  go !_ [] = []
  go 0 (x : xs) = x : go (n - 1) xs
  go k (_ : xs) = go (k - 1) xs
{-# INLINE everyNth #-}

-- | Divide up a list into N pieces fairly. Walking each list in the
-- result will walk the original list.
splatter :: Int -> [a] -> [[a]]
splatter n = map (everyNth n) . take n . tails
{-# INLINE splatter #-}


parMakeBags :: Ord a => [[a]] -> Eval [Bag a]
parMakeBags xs =
  traverse (rpar . M.fromListWith (+)) $ map (,1) <$> xs
{-# INLINE parMakeBags #-}


parMergeBags_ :: Ord a => [Bag a] -> Eval (Bag a)
parMergeBags_ [] = pure M.empty
parMergeBags_ [t] = pure t
parMergeBags_ q = parMergeBags_ =<< go q where
  go [] = pure []
  go [t] = pure [t]
  go (t1:t2:ts) = (:) <$> rpar (M.unionWith (+) t1 t2) <*> go ts
{-# INLINE parMergeBags_ #-}




parUnionWith
  :: Ord k
  => (v -> v -> v)
  -> Int -- Number of threads to spark
  -> Map k v
  -> Map k v
  -> Eval (Map k v)
parUnionWith f n t1 t2 | n <= 1 = rseq $ M.unionWith f t1 t2
parUnionWith _ !_ Tip t2 = rseq t2
parUnionWith _ !_ t1 Tip = rseq t1
parUnionWith f n (Bin _ k1 x1 l1 r1) t2 = case splitLookup k1 t2 of
  (l2, mb, r2) -> do
    l1l2 <- parEval $ parUnionWith f (n `quot` 2) l1 l2
    r1r2 <- parUnionWith f (n `quot` 2) r1 r2
    case mb of
      Nothing -> rseq $ link k1 x1 l1l2 r1r2
      Just x2 -> rseq $ link k1 fx1x2 l1l2 r1r2
        where !fx1x2 = f x1 x2
{-# INLINE parUnionWith #-}


-- Uses the given number of capabilities per merge, initially,
-- doubling for each round.
parMergeBags :: Ord a => Int -> [Bag a] -> Eval (Bag a)
parMergeBags !_ [] = pure M.empty
parMergeBags !_ [t] = pure t
parMergeBags n q = parMergeBags (n * 2) =<< go q where
  go [] = pure []
  go [t] = pure [t]
  go (t1:t2:ts) = (:) <$> parEval (parUnionWith (+) n t1 t2) <*> go ts
{-# INLINE parMergeBags #-}


parMerge :: Ord a => [[a]] -> Eval [a]
parMerge xs = do
  bags <- parMakeBags xs
  -- Why 2 and not one? We only have half as many
  -- pairs as we have lists (capabilities we want to use)
  -- so we double up.
  m <- parMergeBags 2 bags
  pure $ concat [replicate c x | (x,c) <- M.toList m]
{-# INLINE parMerge #-}


pSort :: Ord a => Int -> [a] -> [a]
pSort n = runEval . parMerge . splatter n
{-# INLINE pSort #-}


-- | Force the first n conses of a list
walkList :: Int -> [a] -> ()
walkList n _ | n <= 0 = ()
walkList _ [] = ()
walkList n (_:xs) = walkList (n - 1) xs
{-# INLINE walkList #-}



-- | Use up to the given number of threads to convert a bag
-- to a list, appending the final list argument.
parToListPlus :: Int -> Bag k -> [k] -> Eval [k]
parToListPlus n m lst | n <= 1 = do
  rseq (walkList (M.size m) res)
  pure res
  -- Note: the concat and ++ should fuse away when compiling with
  -- optimization.
  where res = concat [replicate c x | (x,c) <- M.toList m] ++ lst
parToListPlus _ Tip lst = pure lst
parToListPlus n (Bin _ x c l r) lst = do
  r' <- parEval $ parToListPlus (n `quot` 2) r lst
  res <- parToListPlus (n `quot` 2) l $ replicate c x ++ r'
  _ <- rseq r' -- make sure the right side is finished
  pure res
{-# INLINE parToListPlus #-}


parBags :: Ord a => Int -> [a] -> Bag a
parBags n xs = runEval $ do
  bags <- parMakeBags $ splatter n xs
  parMergeBags_ bags
{-# INLINE parBags #-}
