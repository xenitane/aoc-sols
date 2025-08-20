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

import Data.List (sort)
import Data.List.Split (splitOn)

solve :: String -> String
solve input = (first, second) |> pairToStr
  where
    first = blockedRanges |> findSmallestUnblocked
    second = blockedRanges |> mergeRanges |> countBlocked |> (4294967296 -)
    blockedRanges =
        input |> lines |> map (splitOn "-" =:> map (read @Int)) |> sort

countBlocked :: [[Int]] -> Int
countBlocked [] = 0
countBlocked ([a, b]:rest) = b - a + 1 + countBlocked rest

mergeRanges :: [[Int]] -> [[Int]]
mergeRanges [] = []
mergeRanges [a] = [a]
mergeRanges ([a, b]:[c, d]:rest) =
    if c > b + 1
        then [a, b] : mergeRanges ([c, d] : rest)
        else mergeRanges ([a, max b d] : rest)

findSmallestUnblocked :: [[Int]] -> Int
findSmallestUnblocked [[a, b]] =
    if a == 0
        then b + 1
        else 0
findSmallestUnblocked ([a, b]:[c, d]:rest)
    | a > 0 = 0
    | a == 0 && c > b + 1 = b + 1
    | a == 0 && c <= b + 1 = findSmallestUnblocked ([0, max b d] : rest)

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
