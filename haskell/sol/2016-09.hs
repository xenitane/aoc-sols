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

import Data.Char (isDigit)

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) = decompress input

decompress :: String -> (Int, Int)
decompress "" = (0, 0)
decompress ('(':rest) =
    let rawRest = rest |> dropWhile (/= ')') |> drop 1
     in rawRest |> drop len |> decompress |>
        boths (+) ((* times) $* (len, rawRest |> take len |> decompress |> snd))
  where
    times = rest |> dropWhile isDigit |> drop 1 |> takeWhile isDigit |> read
    len = rest |> takeWhile isDigit |> read
decompress (h:rest) = rest |> decompress |> (succ $*)

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
