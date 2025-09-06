module Sol where

import Lib

solve :: String -> String
solve input = (first, second) |> pairToStr
  where
    first = input |> read |> findWinner0
    second = input |> read |> findWinner1

findWinner1 :: Int -> Int
findWinner1 1 = 1
findWinner1 2 = 1
findWinner1 n =
    let t = 1 + div n 2
     in case findWinner1 (n - 1) of
            k
                | k + 1 < t -> k + 1
                | k + 2 <= n -> k + 2
                | otherwise -> 1

findWinner0 :: Int -> Int
findWinner0 n =
    let k = n - ge2ltn 1 n
     in k * 2 + 1

ge2ltn :: Int -> Int -> Int
ge2ltn r n
    | r > n = div r 2
    | otherwise = ge2ltn (r * 2) n
