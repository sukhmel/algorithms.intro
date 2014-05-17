{- |
Module      : $Header$
Description : Selection sort.
Copyright   : (c) Sukhmel Vladislav
License     : MIT

Maintainer  : sukhmel.v@gmail.com
Stability   : unstable
Portability : portabile

Excercise 2.2.2, selection sort by given selection function.
-}

module SelectionSort where

import Data.List (delete)
import SortingTests

-- | implementation of selection by predicate sort
selectionSortBy       :: Eq a => ([a] -> a) -> [a] -> [a]
selectionSortBy _ []  = []
selectionSortBy _ [x] = [x]
selectionSortBy f xs  = x : selectionSortBy f rest
          where x     = f xs        -- an element is selected by a predicate and
                rest  = delete x xs -- then removed from the rest of list

-- | perform tests with selection sort using quickcheck
main :: IO ()
main = perform (minimum, maximum) selectionSortBy
