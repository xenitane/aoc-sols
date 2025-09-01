module Sol where

import Lib

import Data.List

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) =
        let accFunc a = isValidTriangle =:> (+ a)
         in input
                |> lines
                |> map (words =:> map read)
                |> zz transform
                |> foldl (boths accFunc) (0, 0)

zz :: ([a] -> [a]) -> [a] -> [Pair a]
zz f a = zip a (f a)

transform :: [[Int]] -> [[Int]]
transform [] = []
transform (t0:t1:t2:rest) = transposeTrio [t0, t1, t2] ++ transform rest

transposeTrio :: [[Int]] -> [[Int]]
transposeTrio [[a0, b0, c0], [a1, b1, c1], [a2, b2, c2]] =
    [[a0, a1, a2], [b0, b1, b2], [c0, c1, c2]]

isValidTriangle :: [Int] -> Int
isValidTriangle triangle =
    let [a, b, c] = sort triangle
     in if a + b > c
            then 1
            else 0
