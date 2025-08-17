{-# LANGUAGE CPP #-}

import Data.List (elemIndex)
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import Lib (exit, pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) =
        (foldl (\(p, p') ip -> (p + f ip, p' + f' ip)) (0, 0) . lines) input
    f = boolToInt . supportsTLS False False
    f' = boolToInt . supportsSSL (Set.empty, Set.empty)

boolToInt :: Bool -> Int
boolToInt x =
    if x
        then 1
        else 0

supportsSSL :: (Set String, Set String) -> String -> Bool
supportsSSL _ [_, _] = False
supportsSSL (s, s') ('[':rest) = supportsSSL (s', s) rest
supportsSSL (s, s') (']':rest) = supportsSSL (s', s) rest
supportsSSL (s, s') (b0:b1:b2:rest) =
    if b0 == b2 && b0 /= b1
        then Set.member [b1, b0, b1] s' ||
             supportsSSL (Set.insert [b0, b1, b2] s, s') (b1 : b2 : rest)
        else supportsSSL (s, s') (b1 : b2 : rest)

supportsTLS :: Bool -> Bool -> String -> Bool
supportsTLS _ supernet [_, _, _] = supernet
supportsTLS False shnet ('[':rest) = supportsTLS True shnet rest
supportsTLS True shnet (']':rest) = supportsTLS False shnet rest
supportsTLS False supernet (b0:b1:b2:b3:rest) =
    supportsTLS
        False
        (supernet || (b0 == b3 && b0 /= b1 && b1 == b2))
        (b1 : b2 : b3 : rest)
supportsTLS True supernet (b0:b1:b2:b3:rest) =
    (b0 /= b3 || b0 == b1 || b1 /= b2) &&
    supportsTLS True supernet (b1 : b2 : b3 : rest)

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
