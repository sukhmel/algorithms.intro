{- |
Module      : $Header$
Description : Chapter 4.2 matrix products.
Copyright   : (c) Sukhmel Vladislav
License     : MIT

Maintainer  : sukhmel.v@gmail.com
Stability   : unstable
Portability : portabile

Different kinds of matrix products, that are assumed to be of sizes
A = n x k, and B = k x n.
-}

module MatrixProduct where

import Data.List
import Data.List.Grouping
import Test.QuickCheck

type Matrix a = [[a]]

size   :: Matrix a -> (Int, Int)
size x = (length x, length . head $ x)

-- | Straightforward implementation of matrix product.
simpleProduct     :: Num a
                  => Matrix a
                  -> Matrix a
                  -> Matrix a
simpleProduct a b = if (snd . size) a /= (fst . size) b
                       then undefined
                       else let b' = transpose b
                            in  [[sum . zipWith (*) x $ y
                                                | y <- b']
                                                | x <-  a]

-- | Make a 3x3 matrix out of a list.
prepare3x3 :: [a] -> Matrix a
prepare3x3 = take 3 . splitEvery 3

-- | Matrix is same if multiplied by E matrix from both sides.
prop_EMatrix :: [Int] -> Property
prop_EMatrix xs = length xs > 9
                ==> x == simpleProduct e x
                &&  x == simpleProduct x e
        where x = prepare3x3 xs
              e = [ [1, 0, 0]
                  , [0, 1, 0]
                  , [0, 0, 1] ]

-- | Property of reversing by left or right product on reversing matrix.
prop_Reverse :: [Int] -> Property
prop_Reverse xs = length xs > 9
                ==>    reverse x == simpleProduct r x
                && map reverse x == simpleProduct x r
        where x = prepare3x3 xs
              r = [ [0, 0, 1]
                  , [0, 1, 0]
                  , [1, 0, 0] ]


