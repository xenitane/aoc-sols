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
