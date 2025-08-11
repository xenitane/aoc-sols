{-# LANGUAGE CPP #-}

import Data.List (sort)
import Lib (pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

solve :: String -> (Int, Int)
solve input = (first, second)
  where
    first = (length . filter id . map isValidTriangle) triangles1
    second = (length . filter id . map isValidTriangle) triangles2
    triangles1 =
        (map (sort . map (\len -> read len :: Int) . words) . lines) input
    triangles2 =
        (transform . map (map (\len -> read len :: Int) . words) . lines) input

transform :: [[Int]] -> [[Int]]
transform [] = []
transform (a:b:c:r) = makeTriangles a b c ++ transform r

makeTriangles :: [Int] -> [Int] -> [Int] -> [[Int]]
makeTriangles [a, b, c] [d, e, f] [g, h, i] =
    [sort [a, d, g], sort [b, e, h], sort [c, f, i]]

isValidTriangle :: [Int] -> Bool
isValidTriangle [a, b, c] = a + b > c

inputFilePath :: FilePath
inputFilePath = "../inputs/" ++ YEAR ++ "-" ++ DAY ++ ".txt"

testInputFilePath :: FilePath
testInputFilePath = "../test_inputs/" ++ YEAR ++ "-" ++ DAY ++ ".txt"

testOutputFilePath :: FilePath
testOutputFilePath = "../test_outputs/" ++ YEAR ++ "-" ++ DAY ++ ".txt"

runNormalMode :: IO ()
runNormalMode = do
    input <- safeReadFile inputFilePath
    putStr $ pairToStr $ solve input

runTestMode :: IO ()
runTestMode = do
    input <- safeReadFile testInputFilePath
    expected <- trimTrailing <$> safeReadFile testOutputFilePath
    let actual = trimTrailing $ pairToStr $ solve input
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
            exitWith $ ExitFailure 1

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
    exitWith $ ExitFailure 1
#endif
