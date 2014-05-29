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

type Matrix  a = [[a]]
type MatFunc a =  Matrix a
               -> Matrix a
               -> Matrix a

size   :: Matrix a -> (Int, Int)
size x = (length x, length . head $ x)

-- | Straightforward implementation of matrix product.
simpleProduct     :: Num a
                  => MatFunc a
simpleProduct a b = if (snd . size) a /= (fst . size) b
                       then undefined
                       else let b' = transpose b
                            in  [[sum . zipWith (*) x $ y
                                                | y <- b']
                                                | x <-  a]

-- | Split matrix into its submatrices, i. e.
--       / A_00  A_01 \
--   A = |            |
--       \ A_10  A_11 /
splitInFour          :: Matrix a
                     -> ( Matrix a
                        , Matrix a
                        , Matrix a
                        , Matrix a )
splitInFour       a  = (a00, a01, a10, a11)
    where (  n,   m) = size a
          ( a0,  a1) = splitAt (n `div` 2) a
          (a00, a01) = piecesOf a0
          (a10, a11) = piecesOf a1
          piecesOf   = foldr (\ (v, w) (b, c)
                               -> (v:b, w:c)) ([],[])
                     . map (splitAt $ m `div` 2)

-- | Inverse of previous function so that joinFromFour . splitInFour == id
joinFromFour                      :: ( Matrix a
                                     , Matrix a
                                     , Matrix a
                                     , Matrix a )
                                  -> Matrix a
joinFromFour (a00, a01, a10, a11) = a
    where a0 = zipWith (++) a00 a01
          a1 = zipWith (++) a10 a11
          a  = a0 ++ a1

splitProduct  :: Num a
              => MatFunc a
splitProduct a b = if small a || small b
                      then simpleProduct a b
                      else joinFromFour (c00, c01, c10, c11)
    where small  = (\ (x, y) -> x < 2 || y < 2) . size
          c00    = splitProduct a00 b00 `plus`
                   splitProduct a01 b10
          c01    = splitProduct a00 b01 `plus`
                   splitProduct a01 b11
          c10    = splitProduct a10 b00 `plus`
                   splitProduct a11 b10
          c11    = splitProduct a10 b01 `plus`
                   splitProduct a11 b11

          plus   = zipWith (zipWith (+))

          (a00, a01, a10, a11) = splitInFour a
          (b00, b01, b10, b11) = splitInFour b

-- | Make an NxN matrix out of a list.
prepareNxN   :: Int -> [a] -> Matrix a
prepareNxN n = take n . splitEvery n

-- | Make a 3x3 matrix out of a list.
prepare3x3 :: [a] -> Matrix a
prepare3x3 = prepareNxN 3

-- | Matrix is same if multiplied by E matrix from both sides.
prop_EMatrix :: MatFunc Int -> [Int] -> Property
prop_EMatrix f xs = length xs > 9
                  ==> x == f e x
                  &&  x == f x e
        where x = prepare3x3 xs
              e = [ [1, 0, 0]
                  , [0, 1, 0]
                  , [0, 0, 1] ]

-- | Property of reversing by left or right product on reversing matrix.
prop_Reverse :: MatFunc Int -> [Int] -> Property
prop_Reverse f xs = (not . null . drop 9) xs
                  ==>    reverse x == f r x
                  && map reverse x == f x r
        where x = prepare3x3 xs
              r = [ [0, 0, 1]
                  , [0, 1, 0]
                  , [1, 0, 0] ]

-- | Make sure that splitInFour is an inverse of joinFromFour
prop_CorrectSplit :: Int -> [Int] -> Property
prop_CorrectSplit n xs =  n > 0 && (not . null . drop (n' ^2)) xs
                       ==>  x == (joinFromFour . splitInFour) x
              where n' = n `mod` 7 + 1
                    x  = prepareNxN n' xs

-- | Make sure that splitInFour is an inverse of joinFromFour
prop_SameProd :: Int -> [Int] -> [Int] -> Property
prop_SameProd n xs ys =  n > 0 && ok xs && ok ys
                       ==> simpleProduct x y
                       ==  splitProduct  x y
              where n' = n `mod` 7 + 1
                    x  = prepareNxN n' xs
                    y  = prepareNxN n' ys
                    ok = (not . null . drop (n' ^2))
