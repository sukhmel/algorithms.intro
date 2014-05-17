{- |
Module      : $Header$
Description : Tests for sort functions.
Copyright   : (c) Sukhmel Vladislav
License     : MIT

Maintainer  : sukhmel.v@gmail.com
Stability   : unstable
Portability : portabile

Automatic  testing  sort functions for some  indicators  of correct  performing,
using [Int] lists.
Tested properties are  correct order of elements after sorting,  correct length,
reversed order upon using opposite predicate for sorting, and the fact that all
elements of original list are elements of sorted one and vice versa.
-}

module SortingTests where

import Test.QuickCheck

prop_Reversible,
 prop_Contanity,
 prop_Length,
 prop_Order    :: (a,a)
               -> ( a
                 -> [Int]
                 -> [Int])
               -> [Int]
               -> Bool

-- | test that negated predicate gives reverse order
prop_Reversible (lt,gt) f xs = f gt xs
                   == reverse (f lt xs)

-- | test that length of a list is preserved
prop_Length     (lt, _) f xs = length xs
                   == length  (f lt xs)

-- | test that all elements of original list are in result and vice versa
prop_Contanity  (lt, _) f xs = ok (xs, rs)
                            && ok (rs, xs)
    where ok (a,b) = (`elem` a) `all` b
          rs       = f lt xs

-- test correct resulting order when sorted descending and ascending
prop_Order (lt,gt) f xs = all test
                              [ ((<=), lt)
                              , ((>=), gt) ]
    where test (check, ord) = let (r:rs) = f ord xs
                              in and $ zipWith check (r:rs) rs

-- | discard unsatisfying tests
nonNullTest :: Testable prop
            => ( [a]
               -> prop)
            -> [a]
            -> Property
nonNullTest t xs = (not . null $ xs) ==> t xs

-- | run all tests with specified (less, greater) pair and tested function
perform :: (a,a)
        -> ( a
          -> [Int]
          -> [Int])
        -> IO ()
perform o f = do
       mapM_ ( quickCheck
             . nonNullTest
             . ($f)
             . ($o) )
           [ prop_Order ]
       mapM_ ( quickCheck
             . ($f)
             . ($o) )
           [ prop_Reversible
           , prop_Contanity
           , prop_Length    ]
