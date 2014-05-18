{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : $Header$
Description : Merge sort and invertion counter.
Copyright   : (c) Sukhmel Vladislav
License     : MIT

Maintainer  : sukhmel.v@gmail.com
Stability   : unstable
Portability : portabile

Merge sort implementation, and counter of inversions in a list, using same
monadic algorithm.
-}

module MergeSort where

import SortingTests
import Control.Monad.Writer

inversions ::  MonadWriter [(t, t)] m => t -> [t] -> m ()
inversions i js = writer ((), [(i, j) | j <- js])

mergeBy :: MonadWriter [(Int, Int)] m
        => (a -> a -> Bool)
        ->   [(a, Int)]
        ->   [(a, Int)]
        -> m [(a, Int)]
mergeBy _ [] b = return b
mergeBy _ a [] = return a
mergeBy f (a:as)
          (b:bs) = if fst a `f` fst b
                      then do rest <- mergeBy f as (b:bs)
                              return $ a:rest
                      else do inversions (snd b) $ map snd (a:as)
                              rest <- mergeBy f (a:as) bs
                              return $ b:rest

mergeSortBy       :: (a -> a -> Bool) -> [a] -> [a]
mergeSortBy f = map fst . fst . runWriter . mergeSortWrapper f

-- | Task 2.4 (e) count all invesions in a list pairs (i, j) where i < j, but
-- a[j] < a[i]
countInverses :: Ord a => [a] -> Int
countInverses = length . snd . runWriter . mergeSortWrapper (<)



mergeSortWrapper f xs = performMergeSortBy f $ zip xs [0..]

performMergeSortBy :: MonadWriter [(Int, Int)] m
                   => (a -> a -> Bool)
                   ->   [(a, Int)]
                   -> m [(a, Int)]
performMergeSortBy _ [ ]    = return [ ]
performMergeSortBy _ [a]    = return [a]
performMergeSortBy f xs     = do
              first  <- performMergeSortBy f beg
              second <- performMergeSortBy f end
              mergeBy f first second
    where (beg, end) = splitAt (length xs `div` 2) xs



-- | perform tests with merge sort using quickcheck
main :: IO ()
main = perform ((<),(>)) mergeSortBy