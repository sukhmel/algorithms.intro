{-# LANGUAGE FlexibleContexts #-}

module MergeSort where

import SortingTests
import Control.Monad.Writer

countInversions ::  MonadWriter [t] m => t -> m ()
countInversions i = writer ((), [i])

mergeBy :: MonadWriter [Int] m
        => (a -> a -> Bool)
        -> [a] -> [a] -> m [a]
mergeBy _ [] b = return b
mergeBy _ a [] = return a
mergeBy f (a:as)
          (b:bs) = if a `f` b
                      then do rest <- mergeBy f as (b:bs)
                              return $ a:rest
                      else do countInversions $ length as + 1
                              rest <- mergeBy f (a:as) bs
                              return $ b:rest

mergeSortBy       :: (a -> a -> Bool) -> [a] -> [a]
mergeSortBy f = fst . runWriter . performMergeSortBy f

-- | Task 2.4 (e) count all invesions in a list pairs (i, j) where i < j, but
-- a[j] < a[i]
countInverses :: Ord a => [a] -> Int
countInverses = sum . snd . runWriter . performMergeSortBy (<)

performMergeSortBy :: MonadWriter [Int] m
                   => (a -> a -> Bool)
                   -> [a] -> m [a]
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
