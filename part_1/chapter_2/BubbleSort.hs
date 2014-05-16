module BubbleSort where

import Data.Maybe
import Control.Monad

import SortingTests

type Predicate a = a -> a -> Bool

-- | Implementation of modified bubble sorting, where reverses let achieve
-- using of pattern matching and avoid use of (++) in the inner loop.
bubbleSortBy ::  (a -> a -> Bool) -> [a] -> [a]
bubbleSortBy f as = case innerSort $ reverse as of
                         Nothing -> as
                         Just v  -> let (x:xs) = reverse v
                                   in x : bubbleSortBy f xs
    where innerSort (a:b:cs) = if b `f` a
                                  then liftM (a:) $ innerSort (b:cs)
                                  else Just $ b : fromMaybe (a:cs)
                                                (innerSort $ a:cs)
          innerSort _        = Nothing

-- | Helper function to sort only N first elements of a list (to avoid
-- splitting and recreating a list like in other implementations)
bubbleSortNBy :: Int -> Predicate a -> [a] -> [a]
bubbleSortNBy n f as = case innerSort n as of
                            Nothing -> as
                            Just xs -> bubbleSortNBy (n-1) f xs
    where innerSort 1     _    = Nothing
          innerSort m (a:b:cs) = if a `f` b
                                    then liftM (a:) $ innerSort (m-1) (b:cs)
                                    else Just $ b : fromMaybe (a:cs)
                                            (innerSort (m-1) $ a:cs)
          innerSort _     _    = Nothing

-- | Implementation that sorts only first N - i elements on i-th loop.
-- This one is still slower than first one though.
bubbleSortBy'' ::  Predicate a -> [a] -> [a]
bubbleSortBy'' f as = bubbleSortNBy (length as) f as

-- | Implementation of proper bubble sorting with reducing inner loop and
-- with no reverses. This version seems to be slower, than above one.
bubbleSortBy' ::  (a -> a -> Bool) -> [a] -> [a]
bubbleSortBy' f xs = case innerSort xs of
                         Nothing -> xs
                         Just v  -> let (y:ys) = v
                                   in y : bubbleSortBy' f ys
    where innerSort as = if n < 2
                            then Nothing
                            else let (bs, [a]) = splitAt (n-1) as
                                     (cs, [b]) = splitAt (n-2) bs
                                  in if b `f` a
                                        then liftM (++[a]) $ innerSort bs
                                        else Just $ fromMaybe (cs++[a])
                                                  (innerSort $ cs++[a]) ++ [b]
               where n = length as

main :: IO ()
main = mapM_ (perform ((<),(>))) [ bubbleSortBy''
                                 , bubbleSortBy'
                                 , bubbleSortBy ]
