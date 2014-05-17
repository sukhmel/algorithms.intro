{- |
Module      : $Header$
Description : Binary search implementation
Copyright   : (c) Sukhmel Vladislav
License     : MIT

Maintainer  : sukhmel.v@gmail.com
Stability   : unstable
Portability : portabile

Excercise 2.3.5, implementation of binary search in sorted list, based on
ordering of list and a comparing predicate.
-}

module BinarySearch where

import Data.List(sort)
import Data.Maybe(isNothing, fromMaybe)
import Test.QuickCheck

type Predicate a = a -> Ordering

-- | Implementation of search for an element in list that must be sorted
-- in given order, by given predicate (returning Ordiring).
binarySearchInBy      :: Ordering
                      -> Predicate a
                      -> [a] -> Maybe Int
binarySearchInBy ord f as = search $ zip as [0..]
        where search []       = Nothing
              search [(v, i)] = if EQ == f v
                                   then Just i
                                   else Nothing
              search xs       = search $ if ord == f v
                                            then less
                                            else more
                    where (less, more) = splitAt (n `div` 2) xs
                          v            = fst . head $ more
                          n            = length xs

-- | Default search in ascending sorted list by simple comparation.
binarySearch ::  Ord a => a -> [a] -> Maybe Int
binarySearch a = binarySearchInBy LT (compare a)

--------------------------------------------------------------------------------
--  Auto-tests

-- | Test ensuring that for existing element, position found holds same value
-- as original.
prop_Included      :: Int -> [Int] -> Property
prop_Included a as' = a `elem` as    ==>
                      a == as !! r
           where as = sort as'
                 r  = fromMaybe (-1) $ binarySearchInBy LT (compare a) as

-- | Test ensuring that for non-existing element search Nothing is returned.
prop_Excluded      :: Int -> [Int] -> Property
prop_Excluded a as' = a `notElem` as ==>
                      isNothing      $ binarySearchInBy LT (compare a) as
           where as = sort as'

-- | Perform all tests
main :: IO ()
main = mapM_ quickCheck [ prop_Included
                        , prop_Excluded ]
