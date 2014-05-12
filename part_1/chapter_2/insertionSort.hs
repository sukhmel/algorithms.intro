{-# LANGUAGE ScopedTypeVariables #-}

module InsertSort where

import Data.List (sort)

-- Implementation of insertion sorting. At each step beginning of a list is
-- already  sorted,  first  element of  ending  is taken  and inserted into
-- beginning in a way that retains order.
insertionSortBy :: forall a. (a -> a -> Bool) -> [a] -> [a]
insertionSortBy f = innerSort 2
              where innerSort      :: Int -> [a] -> [a] -- sort n first elements
                    innerSort _ [] = []                 -- and then sort  n+1
                    innerSort n xs =                    -- in case n is still
                           if n > length xs             -- less  or  equal to
                              then xs                   -- length of the list
                              else innerSort (n+1)
                                 $ e `insertInto` begin
                                           ++ rest
                             where (begin, end) = splitAt (n-1) xs
                                   e            = head end
                                   rest         = tail end

                    insertInto          :: a -> [a] -> [a] -- insert an element
                    e `insertInto` es@(a:as) =             -- into  the list by
                              let c          = last es     -- given predicate.
                                  rest       = init es
                              in  case as of
                                       [] -> if e `f` a then [e, a] else [a, e]
                                       _  -> if e `f` c
                                               then e `insertInto` rest ++ [c]
                                               else es ++ [e]
                    e `insertInto` []        = [e]

-- ensure that flipping of predicate creates reversed result (although
-- that's not really true for predicates same as their flip)
prop_Negation    :: [Int] -> Bool
prop_Negation xs = insertionSortBy (flip (<)) xs ==
          reverse (insertionSortBy (<)        xs)

-- ensure that there is same amount of elements
prop_Length      :: [Int] -> Bool
prop_Length   xs = length xs == length (insertionSortBy (<) xs)

-- ensure elements are same after sorting
prop_Safety      :: [Int] -> Bool
prop_Safety   xs = sort xs == sort (insertionSortBy (<) xs)

-- a little cheat ;-)
prop_Correct     :: [Int] -> Bool
prop_Correct  xs = sort xs == insertionSortBy (<) xs
