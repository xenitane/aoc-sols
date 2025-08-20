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

infinity :: Int
infinity = 9223372036854775807

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
                        str |> splitter " ." |> (((!! 3), (!! 11)) *$) |>
                        (read @Int $*)
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
            then pStr' "test passes\n"
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
