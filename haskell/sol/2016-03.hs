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
        let accFunc a = (+ a) . isValidTriangle
         in (foldl (boths accFunc) (0, 0) .
             zz transform . map (map read . words) . lines)
                input

zz :: ([a] -> [a]) -> [a] -> [Pair a]
zz f a = zip a (f a)

boths :: (a -> b -> c) -> Pair a -> Pair b -> Pair c
boths f (a, a') (b, b') = (f a b, f a' b')

transform :: [[Int]] -> [[Int]]
transform [] = []
transform triangles =
    (transpose . take 3) triangles ++ (transform . drop 3) triangles

transpose :: [[Int]] -> [[Int]]
transpose [[a0, b0, c0], [a1, b1, c1], [a2, b2, c2]] =
    [[a0, a1, a2], [b0, b1, b2], [c0, c1, c2]]

isValidTriangle :: [Int] -> Int
isValidTriangle triangle =
    let [a, b, c] = sort triangle
     in if a + b > c
            then 1
            else 0

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
