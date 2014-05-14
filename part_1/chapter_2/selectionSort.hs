-- task 2.2.2

module SelectionSort where

import Data.List (delete)
import Test.QuickCheck

-- implementation of selection by predicate sort
selectionSortBy       :: Eq a => ([a] -> a) -> [a] -> [a]
selectionSortBy _ []  = []
selectionSortBy _ [x] = [x]
selectionSortBy f xs  = x : selectionSortBy f rest
          where x     = f xs        -- an element is selected by a predicate and
                rest  = delete x xs -- then removed from the rest of list

-- test that an opposite predicate gives reversed list
-- set up in order to copy tests to other files
tested  :: Eq a => ([a] -> a) -> [a] -> [a]
lt, gt  ::  [Int] -> Int
tested  = selectionSortBy
(lt,gt) = (minimum, maximum)

-- test that negated predicate gives reverse order
prop_Reversible    :: [Int] -> Bool
prop_Reversible xs = tested gt xs
         == reverse (tested lt xs)

-- test that length of a list is preserved
prop_Length        :: [Int] -> Bool
prop_Length     xs = length xs
         == length  (tested lt xs)

-- test that all elements of original list are in result and vice versa
prop_Contanity     :: [Int] -> Bool
prop_Contanity  xs = ok (xs, rs) && ok (rs,xs)
    where ok (a,b) = (`elem` a) `all` b
          rs       = tested lt xs

-- test correct resulting order when sorted ascending
prop_OrderAsc    :: [Int] -> Property
prop_OrderAsc  x = (not . null $ x)
            ==> and $ zipWith (<=) (r:rs) rs
    where (r:rs) = tested lt x

-- test correct resulting order when sorted descending
prop_OrderDesc   :: [Int] -> Property
prop_OrderDesc x = (not . null $ x)
            ==> and $ zipWith (>=) (r:rs) rs
    where (r:rs) = tested gt x

-- run all tests
main :: IO ()
main = do
       mapM_ quickCheck [ prop_OrderDesc
                        , prop_OrderAsc  ]
       mapM_ quickCheck [ prop_Reversible
                        , prop_Contanity
                        , prop_Length    ]

