{-# LANGUAGE CPP #-}

import Lib
    ( Pair
    , ($*)
    , (*$)
    , (*$*)
    , (=:>)
    , (|>)
    , block
    , boths
    , exit
    , logId
    , pStr
    , pStr'
    , pairToStr
    , sLogId
    , safeReadFile
    , setAt
    , trimTrailing
    )

import Data.List.Split (condense, dropBlanks, dropDelims, oneOf, split)
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
         in input |> lines |> drop 2 |>
            map (splitter " -%Txy" =:> drop 1 =:> map read)

countViablePairs :: [[Int]] -> Int
countViablePairs drives =
    let filterFunc i u (j, [_, _, _, _, a, _]) = i /= j && u > 0 && u <= a
        sumFunc p (i, [_, _, _, u, _, _]) =
            drives |> zip [1 ..] |> filter (filterFunc i u) |> length |> (+ p)
     in drives |> zip [1 ..] |> foldl sumFunc 0

main :: IO ()
#if defined YEAR && defined DAY
suff :: FilePath
suff = "/" ++ YEAR ++ "-" ++ DAY ++ ".txt"
#if !defined TEST_MODE
main = do
    input <- safeReadFile $ "../inputs" ++ suff
    input |> solve |> pStr
#else
main = do
    input <- safeReadFile $ "../test_inputs" ++ suff
    expected' <- safeReadFile $ "../test_outputs" ++ suff
    let actual = input |> solve |> trimTrailing
        expected = trimTrailing expected'
     in if actual == expected
            then pStr' "test passed\n"
            else do
                pStr' "test failed\n"
                block "Expected" expected
                block "Actual" actual
                exit 1
#endif
#else
main = do
    pStr' "essential variables not defined"
    exit 1
#endif
