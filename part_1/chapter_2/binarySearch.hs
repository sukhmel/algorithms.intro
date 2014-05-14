-- task 2.3.5

module BinarySearch where

import Data.List(sort)
import Data.Maybe(isNothing, fromMaybe)
import Test.QuickCheck

-- | Implementation of search of an element in list that is given to be sorted
-- in ascending order.
binarySearch      :: (Eq a, Ord a) => a -> [a] -> Maybe Int
binarySearch a as = search $ zip as [0..]
        where search []       = Nothing
              search [(v, i)] = if v == a
                                   then Just i
                                   else Nothing
              search xs       = search $ if a <  v
                                            then less
                                            else more
                    where (less, more) = splitAt (n `div` 2) xs
                          v            = fst . head $ more
                          n            = length xs

-- | Test ensuring that for existing element, position found holds same value
-- as original.
prop_Included      :: Int -> [Int] -> Property
prop_Included a as' = a `elem` as    ==>
                      a == as !! r
           where as = sort as'
                 r  = fromMaybe (-1) $ binarySearch a as

-- | Test ensuring that for non-existing element search Nothing is returned.
prop_Excluded      :: Int -> [Int] -> Property
prop_Excluded a as' = a `notElem` as ==>
                      isNothing      $ binarySearch a as
           where as = sort as'

-- | Perform all tests
main :: IO ()
main = mapM_ quickCheck [ prop_Included
                        , prop_Excluded ]
