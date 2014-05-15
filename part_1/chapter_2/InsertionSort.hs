{-# LANGUAGE ScopedTypeVariables #-}

module InsertSort where

import SortingTests

type Predicate a = a -> a -> Bool
type Splitter  a = [a] -> ([a], [a])
type Modifier  a = [a] -> [a]
type Inserter  a = a -> Modifier a
type InsertBy  a = Predicate a -> Inserter a

-- Implementation of insertion sorting. At each step beginning of a list is
-- already  sorted,  first  element of  ending  is taken  and inserted into
-- beginning in a way that retains order.
insertSortBy       :: forall a. InsertBy  a
                             -> Predicate a
                             -> Modifier  a
insertSortBy ins f = innerSort . (,) []
               where innerSort      :: ([a],[a]) -> [a] -- sort n first elements
                     innerSort (a,[]) = a               -- and then  sort n+1 in
                     innerSort (a, b) = innerSort       -- case n is still  less
                               (ins f e a, es)          -- or equal to length of
                         where (e:es) = b               -- the list

-- | Insertion functions highlighting difference between binary and linear
-- insertion algorithms.
insLinearBy, insBinaryBy :: InsertBy a
insLinearBy = insWithBy (\xs -> splitAt (length xs   -   1) xs)
insBinaryBy = insWithBy (\xs -> splitAt (length xs `div` 2) xs)

-- | Insert  an element into list by splitting using specified splitter and
-- inserting into one of resulting parts based on a predicate given.
insWithBy        :: Splitter  a
                 -> InsertBy  a
insWithBy _  _ e []  = [e]
insWithBy _  f e [v] = if e `f` v
                          then [e, v]
                          else [v, e]
insWithBy sp f e es  = if e `f` head second
                          then insWithBy sp f e first ++ second
                          else first ++ insWithBy sp f e second
        where (first, second) = sp es

-- | perform tests with insertion sort using quickcheck
main :: IO ()
main = mapM_ (perform ((<),(>)) . insertSortBy)
                     [insBinaryBy, insLinearBy]
