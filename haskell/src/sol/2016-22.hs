module Sol where

import Lib

import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set

solve :: String -> String
solve input = (first, second) |> pairToStr
  where
    first = countViablePairs storageDetails
    second = () -- draw the grid and see for yourself
    storageDetails =
        let splitter =
                oneOf =:> dropDelims =:> condense =:> dropBlanks =:> split
         in input
                |> lines
                |> drop 2
                |> map (splitter " -%Txy" =:> drop 1 =:> map read)

countViablePairs :: [[Int]] -> Int
countViablePairs drives =
    let filterFunc i u (j, [_, _, _, _, a, _]) = i /= j && u > 0 && u <= a
        sumFunc p (i, [_, _, _, u, _, _]) =
            drives |> zip [1 ..] |> filter (filterFunc i u) |> length |> (+ p)
     in drives |> zip [1 ..] |> foldl sumFunc 0
