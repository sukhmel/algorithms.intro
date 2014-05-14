{-# LANGUAGE ScopedTypeVariables #-}

module InsertSort where

import SortingTests

-- Implementation of insertion sorting. At each step beginning of a list is
-- already  sorted,  first  element of  ending  is taken  and inserted into
-- beginning in a way that retains order.
insertionSortBy :: forall a. (a -> a -> Bool) -> [a] -> [a]
insertionSortBy f = innerSort . (,) []
              where innerSort      :: ([a],[a]) -> [a] -- sort n first elements
                    innerSort (a,[]) = a               -- and then  sort n+1 in
                    innerSort (a, b) = innerSort       -- case n is still  less
                              (e `insertInto` a, es)   -- or equal to length of
                        where (e:es) = b               -- the list

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

-- | perform tests with merge sort using quickcheck
main :: IO ()
main = perform ((<),(>)) insertionSortBy
