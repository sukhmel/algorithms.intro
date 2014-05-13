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

prop_Reversible    :: [Int] -> Bool
prop_Reversible xs = selectionSortBy maximum xs
         == reverse (selectionSortBy minimum xs)

prop_Length        :: [Int] -> Bool
prop_Length     xs = length xs
         == length  (selectionSortBy minimum xs)

prop_Contanity     :: [Int] -> Bool
prop_Contanity  xs = all (`elem` xs)
                    (selectionSortBy minimum xs)

main :: IO ()
main = mapM_ quickCheck [ prop_Reversible
                        , prop_Contanity
                        , prop_Length   ]
