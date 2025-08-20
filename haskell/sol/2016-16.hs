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

import Data.Char (chr, ord)

diskLength :: Pair Int
diskLength = (272, 35651584)

solve :: String -> String
solve input = (first, second) |> pairToStr
  where
    (first, second) = generateChecksum input $* diskLength

generateChecksum :: String -> Int -> String
generateChecksum str minLen
    | length str >= minLen = str |> take minLen |> makeChecksum
    | otherwise =
        let str' =
                str |> reverse |> map (ord =:> (97 -) =:> chr) |> ("0" ++) |>
                (str ++)
         in generateChecksum str' minLen

makeChecksum :: String -> String
makeChecksum str
    | str |> length |> even = str |> shorten |> makeChecksum
    | otherwise = str

shorten :: String -> String
shorten [] = []
shorten (b0:b1:rest) =
    (if b0 == b1
         then "1"
         else "0") ++
    shorten rest

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
