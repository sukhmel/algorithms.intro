-- task 2.1.3

module LinearSearch where

import Data.Maybe(isNothing, fromMaybe)
import Test.QuickCheck

-- Simple implementation of linear search.
linSearch      :: Eq a => a -> [a] -> Maybe Int
linSearch a as = case rs of
                      []  -> Nothing
                      v:_ -> Just . snd $ v
      where rs = dropWhile ((a /=) . fst) zs
            zs = zip as [0..]

-- Test ensuring that for existing element, position found holds same value
-- as original.
prop_Included      :: Int -> [Int] -> Property
prop_Included a as = a `elem` as    ==>
                     a == as !! r
           where r = fromMaybe (-1) $ linSearch a as

-- Test ensuring that for non-existing element search Nothing is returned.
prop_Excluded      :: Int -> [Int] -> Property
prop_Excluded a as = a `notElem` as ==>
                     isNothing $ linSearch a as

-- Test that found value is first possible
prop_Minimal       :: Int -> [Int] -> Property
prop_Minimal  a as = a `elem` as    ==>
                     a `notElem` take (r-1) as
           where r = fromMaybe (-1) $ linSearch a as

main :: IO ()
main = mapM_ quickCheck [ prop_Included
                        , prop_Excluded
                        , prop_Minimal]
