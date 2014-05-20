{- |
Module      : $Header$
Description : Finding sublist with maximal sum of elements.
Copyright   : (c) Sukhmel Vladislav
License     : MIT

Maintainer  : sukhmel.v@gmail.com
Stability   : unstable
Portability : portabile

Excercises 4.1.2, 4.1.5 - a list is given, task is to find indices, that will
correspond to begin and end of a sublist, that has maximal sum of its elements.
Implementation of algorithms with different efficiency:
    * brute force
    * recursive divide and conquer
    * linear fold-based
-}

module MaximalSublist where

import Test.QuickCheck

-- | O(n^2) implementation using brute force method (although not every
-- possible combination is checked, but only reasonable ones).
bruteForce     :: (Ord a, Num a) => [a] -> (Int, Int)
bruteForce xs  = snd . maxFold $ [(v, (i, j))
                    | j <- [0..length xs - 1]
                    , i <- [0..j]
                    , let v = subsum i j xs ]

-- | Finding maximum by only first element of a tuple.
maxFold, minFold :: Ord a => [(a, b)] -> (a, b)
maxFold = compareFold (<)
minFold = compareFold (>)

-- | Generic function for above folds.
compareFold   :: (a -> a -> Bool) -> [(a, b)] -> (a, b)
compareFold f = foldl1 (\a v -> if fst a `f` fst v
                                  then v
                                  else a)

-- | O(n*lg(n)) solution, using divide and conquer strategy.
divideConquer    :: (Ord a, Num a) => [a] -> (Int, Int)
divideConquer [] = error "Incorrect input"
divideConquer xs = snd . conquer $ zip xs [0..]
    where conquer [(a, i)] = (a, (i, i))
          conquer ys         =
                let (lo, hi) = splitAt (length ys `div` 2) ys
                    left     = conquer lo
                    right    = conquer hi
                    mid      = crossSum (reverse lo) hi
                in maxFold [left, mid, right]
          crossSum a b       =
                let left     = maxFold . scan $ a
                    right    = maxFold . scan $ b
                in (fst left + fst right, (snd left, snd right))
                where scan   = scanl1 (\(n,_) (m,k) -> (n + m, k))

-- | O(n) algorithm, based on step-by-step extension.
-- on each step current best subsum is calculated as one of three alternatives:
--    1) previous subsum (note, that because of preprocessing step, x_i holds
--       sum, corresponding to all indices [0..i] thus when this value is
--       substracted, then resulting index starts with (i+1)
--    2) new subsum with same beginning, but ending in current folded element;
--    3) new subsum, beginning with minimal element found since start
--       (actually, from i, because there could be no smaller elements between
--       0 and i, than i itself), and ending with current element k.
-- minimal element is also tracked while folding goes on.
extendSearch     :: (Ord a, Num a) => [a] -> (Int, Int)
extendSearch [_] = (0,0)
extendSearch ds  = (\((_,i), (_, j), _) -> (i, j))
                 . foldl f ((0,0), (x,0), (x,1))
                 $ zip xs [1..]
    where (x:xs) = scanl1 (+) ds
          f ((x_i,i),(x_j,j),(x_m,m)) (x_k,k) =
                    ((x_i',i'),(x_j',j'),(x_m',m'))
              where (x_m', m') = if x_k < x_m
                                    then (x_k, k+1)
                                    else (x_m, m)
                    (_,((x_i',i'),(x_j',j'))) = maxFold
                        [ (x_k - x_i, ((x_i,i),(x_k,k)))
                        , (x_k - x_m, ((x_m,m),(x_k,k)))
                        , (x_j - x_i, ((x_i,i),(x_j,j)))]

-- | Helper function, returning subsum of a list with given start and end
-- indices.
subsum     :: Num c => Int -> Int -> [c] -> c
subsum i j = sum . take (j - i + 1) . drop i

-- | Test correctness.
prop_Correct      :: ([Int] -> (Int, Int)) -> [Int] -> Property
prop_Correct f xs = not (null xs) ==>
                    uncurry subsum (f xs) xs == maximum
                    [subsum i j xs | i <- [0..length xs - 1]
                                   , j <- [0..length xs - 1]
                                   , j >= i ]

-- | Perform all tests on all methods implemented.
main :: IO ()
main = mapM_ (quickCheck . prop_Correct) [ divideConquer
                                         , extendSearch
                                         , bruteForce  ]
