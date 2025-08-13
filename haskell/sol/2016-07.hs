{-# LANGUAGE CPP #-}

import Data.List (elemIndex)
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import Lib (pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

solve :: String -> (Int, Int)
solve input = (first, second)
  where
    first = (length . filter (supportsTLS False (False, True))) ips
    second = (length . filter (supportsSSL False (Set.empty, Set.empty))) ips
    ips = lines input

supportsSSL :: Bool -> (Set String, Set String) -> String -> Bool
supportsSSL _ _ [_, _] = False
supportsSSL False a ('[':rest) = supportsSSL True a rest
supportsSSL False (a, b) (b0:b1:b2:rest) =
    if b0 == b2 && b0 /= b1
        then Set.member [b1, b0, b1] b ||
             supportsSSL False (Set.insert [b0, b1, b2] a, b) (b1 : b2 : rest)
        else supportsSSL False (a, b) (b1 : b2 : rest)
supportsSSL True a (']':rest) = supportsSSL False a rest
supportsSSL True (a, b) (b0:b1:b2:rest) =
    if b0 == b2 && b0 /= b1
        then Set.member [b1, b0, b1] a ||
             supportsSSL True (a, Set.insert [b0, b1, b2] b) (b1 : b2 : rest)
        else supportsSSL True (a, b) (b1 : b2 : rest)

supportsTLS :: Bool -> (Bool, Bool) -> String -> Bool
supportsTLS _ (_, False) _ = False
supportsTLS _ (a, b) [_, _, _] = a && b
supportsTLS False a ('[':rest) = supportsTLS True a rest
supportsTLS False (a, b) (b0:b1:b2:b3:rest) =
    supportsTLS False (a || c, b) (b1 : b2 : b3 : rest)
  where
    c = b0 == b3 && b0 /= b1 && b1 == b2
supportsTLS True a (']':rest) = supportsTLS False a rest
supportsTLS True (a, b) (b0:b1:b2:b3:rest) =
    supportsTLS True (a, b && c) (b1 : b2 : b3 : rest)
  where
    c = b0 /= b3 || b0 == b1 || b1 /= b2

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
