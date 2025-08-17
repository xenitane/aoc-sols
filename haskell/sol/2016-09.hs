{-# LANGUAGE CPP #-}

import Data.Char (isDigit)
import Data.Tuple.Extra (both)
import Lib (exit, pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) = decompress input

decompress :: String -> (Int, Int)
decompress "" = (0, 0)
decompress ('(':rest) =
    (boths (+) (both (* times) (len, (snd . decompress . take len) rawRest)) .
     decompress)
        (drop len rawRest)
  where
    rawRest = (drop 1 . dropWhile (/= ')')) rest
    times = (read . takeWhile isDigit . drop 1 . dropWhile isDigit) rest
    len = (read . takeWhile isDigit) rest
decompress (h:rest) = (both succ . decompress) rest

boths :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
boths f (a, a') (b, b') = (f a b, f a' b')

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
