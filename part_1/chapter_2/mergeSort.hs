module MergeSort where

import Test.QuickCheck

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

-- set up in order to copy tests to other files
tested  ::  (a -> a -> Bool) -> [a] -> [a]
lt, gt  ::  Int -> Int -> Bool
tested  = mergeSortBy
(lt,gt) = ((<),(>))

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
