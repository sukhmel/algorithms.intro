module MergeSort where

import SortingTests

mergeSortBy       :: (a -> a -> Bool) -> [a] -> [a]
mergeSortBy _ [ ]    = [ ]
mergeSortBy _ [a]    = [a]
mergeSortBy f [a, b] = if a `f` b
                          then [a, b]
                          else [b, a]
mergeSortBy f xs     = mergeSortBy f beg
               `merge` mergeSortBy f end
    where (beg, end) = splitAt (length xs `div` 2) xs
          merge [] b = b
          merge a [] = a
          merge (a:as) (b:bs) = if a `f` b
                                   then a: merge as (b:bs)
                                   else b: merge (a:as) bs

-- | perform tests with merge sort using quickcheck
main :: IO ()
main = perform ((<),(>)) mergeSortBy
