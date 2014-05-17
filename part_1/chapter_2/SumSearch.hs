{- |
Module      : $Header$
Description : Search for a value x in all sums of list elements.
Copyright   : (c) Sukhmel Vladislav
License     : MIT

Maintainer  : sukhmel.v@gmail.com
Stability   : unstable
Portability : portabile

Excercise 2.3.7, search for a possibility (and indices) of taking two elements of a list to
construct sum with a given value.
-}

module SumSearch where

import BinarySearch
import MergeSort

import Data.Maybe(isJust, fromJust)
import Test.QuickCheck

-- | Implementation of task 2.3.7 finds out if it's possible to construct
-- a sum with value of x out of elements from s, takes Theta((n+1)lg(n))
-- time to execute (theoretically).
checkForSum ::  (Num a, Ord a) => a -> [a] -> Bool
checkForSum x s = not . null $ indicesForSum x s

indicesForSum     :: (Num a, Ord a) => a -> [a] -> [(Int, Int)]
indicesForSum x s = map (\ (a, b)
                         -> (a, snd . (!!) vs' . fromJust $ b))
                  . filter (isJust . snd)
                  . map (\(v,i)
                         -> (,) i $
                            binarySearchInBy LT (\(a,b)
                                                -> v `compare` a)
                           vs')
                  $ s'
        where s'  = zip s [0..]
              vs  = map (\(a,b) -> (x-a,b)) s'
              vs' = mergeSortBy (\a b -> fst a < fst b) vs


-- | Correct negative answer when sum is not constructable
prop_CorrectFalse     :: Int -> [Int] -> Property
prop_CorrectFalse x s = x `notElem` s' ==>
                        not $ checkForSum x s
             where s' = [ a + b | a <- s, b <- s ]

-- | Correct positive answer when sum is constructable
prop_CorrectTrue      :: Int -> [Int] -> Property
prop_CorrectTrue  x s = x `elem` s' ==>
                        checkForSum x s
             where s' = [ a + b | a <- s, b <- s ]

-- | Correct indices to construct sum are given as an answer
prop_CorrectIndex     :: Int -> [Int] -> Property
prop_CorrectIndex x s = x `elem` s' ==>
                        all (\(i,j) -> x == s !! i + s !! j) result
         where s'     = [ a + b | a <- s, b <- s ]
               result = indicesForSum x s

main :: IO ()
main = mapM_ quickCheck [ prop_CorrectIndex
                        , prop_CorrectFalse
                        , prop_CorrectTrue]
