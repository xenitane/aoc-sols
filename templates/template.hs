{-# LANGUAGE CPP #-}

import Lib (pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

solve :: String -> ((), ())
solve input = (first, second)
  where
    first = ()
    second = ()

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
