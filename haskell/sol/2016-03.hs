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

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) =
        let accFunc a = isValidTriangle =:> (+ a)
         in input |> lines |> map (words =:> map read) |> zz transform |>
            foldl (boths accFunc) (0, 0)

zz :: ([a] -> [a]) -> [a] -> [Pair a]
zz f a = zip a (f a)

transform :: [[Int]] -> [[Int]]
transform [] = []
transform (t0:t1:t2:rest) = transpose [t0, t1, t2] ++ transform rest

transpose :: [[Int]] -> [[Int]]
transpose [[a0, b0, c0], [a1, b1, c1], [a2, b2, c2]] =
    [[a0, a1, a2], [b0, b1, b2], [c0, c1, c2]]

isValidTriangle :: [Int] -> Int
isValidTriangle triangle =
    let [a, b, c] = sort triangle
     in if a + b > c
            then 1
            else 0

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
