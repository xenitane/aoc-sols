{-# LANGUAGE CPP #-}

import Data.List (sort)
import Lib (exit, pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

type Pair a = (a, a)

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) =
        (foldl (boths (\a b -> a + isValidTriangle b)) (0, 0) .
         zz . ((\x -> (x, transform x)) . map (map read . words) . lines))
            input

zz :: Pair [a] -> [Pair a]
zz (a, b) = zip a b

boths :: (a -> b -> c) -> Pair a -> Pair b -> Pair c
boths f (a0, a1) (b0, b1) = (f a0 b0, f a1 b1)

transform :: [[Int]] -> [[Int]]
transform [] = []
transform triangles =
    (makeTriangles . take 3) triangles ++ (transform . drop 3) triangles

makeTriangles :: [[Int]] -> [[Int]]
makeTriangles [[a, b, c], [d, e, f], [g, h, i]] =
    [[a, d, g], [b, e, h], [c, f, i]]

isValidTriangle :: [Int] -> Int
isValidTriangle triangle =
    if a + b > c
        then 1
        else 0
  where
    [a, b, c] = sort triangle

inputFilePath :: FilePath
inputFilePath = "../inputs/" ++ YEAR ++ "-" ++ DAY ++ ".txt"

testInputFilePath :: FilePath
testInputFilePath = "../test_inputs/" ++ YEAR ++ "-" ++ DAY ++ ".txt"

testOutputFilePath :: FilePath
testOutputFilePath = "../test_outputs/" ++ YEAR ++ "-" ++ DAY ++ ".txt"

runNormalMode :: IO ()
runNormalMode = do
    input <- safeReadFile inputFilePath
    (putStr . solve) input

runTestMode :: IO ()
runTestMode = do
    input <- safeReadFile testInputFilePath
    expectedIO <- safeReadFile testOutputFilePath
    let expected = trimTrailing expectedIO
    let actual = (trimTrailing . solve) input
    if actual == expected
        then putStrLn "test passed"
        else do
            putStrLn "test failed"
            putStrLn "Expected:"
            putStrLn "--------------"
            putStrLn expected
            putStrLn "--------------"
            putStrLn "Got:"
            putStrLn "--------------"
            putStrLn actual
            putStrLn "--------------"
            exit 1

main :: IO ()
#if defined YEAR && defined DAY
main = do
    testMode <- lookupEnv "TEST_MODE"
    case testMode of
        Just "1" -> runTestMode
        _ -> runNormalMode
#else
main = do
    putStrLn "essential variables not defined"
    exit 1
#endif
