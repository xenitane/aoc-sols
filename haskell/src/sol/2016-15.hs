module Sol where

import Lib

import Data.List.Split

solve :: String -> String
solve input = (first, second) |> pairToStr
  where
    first = findTime 0 (prod * 2) slotsStart
    second =
        findTime
            0
            (prod * 22)
            (slotsStart ++ [(11, (length slotsStart + 1) `mod` 11)])
    prod =
        let ff b = fst =:> (* b)
         in foldl ff 1 slotsStart
    slotsStart =
        let splitter =
                oneOf =:> dropDelims =:> condense =:> dropBlanks =:> split
            mapFunc i str =
                let (m, r) =
                        str
                            |> splitter " ."
                            |> (((!! 3), (!! 11)) *$)
                            |> (read @Int $*)
                 in (m, (r + i) `mod` m)
         in input |> lines |> zipWith mapFunc [1 ..]

findTime :: Int -> Int -> [Pair Int] -> Int
findTime now max vals
    | now > max = infinity
    | validNow now vals = now
    | otherwise = findTime (now + 1) max vals

validNow :: Int -> [Pair Int] -> Bool
validNow now vals =
    let ff (m, r) = (r + now) `mod` m == 0
     in all ff vals
