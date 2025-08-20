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
