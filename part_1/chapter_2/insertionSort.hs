{-# LANGUAGE ScopedTypeVariables #-}

module InsertSort where

insertionSortBy :: forall a. (a -> a -> Bool) -> [a] -> [a]
insertionSortBy f = innerSort 2
              where
                    innerSort      :: Int -> [a] -> [a]
                    innerSort _ [] = []
                    innerSort n xs =
                           if n > length xs
                              then xs
                              else innerSort (n+1)
                                 $ drownSort (take n xs)
                                           ++ drop n xs

                    drownSort             :: [a] -> [a]
                    drownSort ys@(a:b:cs) =
                            let begin     = init ys
                                rest      = init begin
                                compared  = last ys
                                candidate = last begin
                            in  case cs of
                                     [] -> if b `f` a then [b, a] else [a, b]
                                     _  -> if compared `f` candidate
                                             then drownSort (rest ++ [compared])
                                                                  ++ [candidate]
                                             else ys
                    drownSort rest = rest
