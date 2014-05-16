module BubbleSort where

import Data.Maybe
import Control.Monad

import SortingTests

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
main = mapM_ (perform ((<),(>))) [ bubbleSortBy'
                                 , bubbleSortBy]
