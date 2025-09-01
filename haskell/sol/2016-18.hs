module Sol where

import Lib

requiredRows :: Pair Int
requiredRows = (40, 400000)

solve :: String -> String
solve input = (first, second) |> pairToStr
  where
    (first, second) = findSafeCells input $* requiredRows

findSafeCells :: String -> Int -> Int
findSafeCells _ 0 = 0
findSafeCells row n =
    (row |> filter (== '.') |> length) + findSafeCells (makeRow row) (n - 1)

makeRow :: String -> String
makeRow row = take (length row) [0 ..] |> map nextCell
  where
    nextCell :: Int -> Char
    nextCell i
        | i == 0 = row !! 1
        | i == length row - 1 = row !! (length row - 2)
        | otherwise =
            let (l, r) = ((!! (i - 1)), (!! (i + 1))) *$ row
             in if l /= r
                    then '^'
                    else '.'
