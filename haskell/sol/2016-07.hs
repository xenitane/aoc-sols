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
        (foldl (\(p0, p1) ip -> (p0 + f0 ip, p1 + f1 ip)) (0, 0) . lines) input
    f0 = boolToInt . supportsTLS False (False, True)
    f1 = boolToInt . supportsSSL False (Set.empty, Set.empty)

boolToInt :: Bool -> Int
boolToInt x =
    if x
        then 1
        else 0

supportsSSL :: Bool -> (Set String, Set String) -> String -> Bool
supportsSSL _ _ [_, _] = False
supportsSSL False shmap ('[':rest) = supportsSSL True shmap rest
supportsSSL True shmap (']':rest) = supportsSSL False shmap rest
supportsSSL False (supernetMap, hypernetMap) (b0:b1:b2:rest) =
    if b0 == b2 && b0 /= b1
        then Set.member [b1, b0, b1] hypernetMap ||
             supportsSSL
                 False
                 (Set.insert [b0, b1, b2] supernetMap, hypernetMap)
                 (b1 : b2 : rest)
        else supportsSSL False (supernetMap, hypernetMap) (b1 : b2 : rest)
supportsSSL True (supernetMap, hypernetMap) (b0:b1:b2:rest) =
    if b0 == b2 && b0 /= b1
        then Set.member [b1, b0, b1] supernetMap ||
             supportsSSL
                 True
                 (supernetMap, Set.insert [b0, b1, b2] hypernetMap)
                 (b1 : b2 : rest)
        else supportsSSL True (supernetMap, hypernetMap) (b1 : b2 : rest)

supportsTLS :: Bool -> (Bool, Bool) -> String -> Bool
supportsTLS _ (_, False) _ = False
supportsTLS _ (supernet, hypernet) [_, _, _] = supernet && hypernet
supportsTLS False shnet ('[':rest) = supportsTLS True shnet rest
supportsTLS True shnet (']':rest) = supportsTLS False shnet rest
supportsTLS False (supernet, hypernet) (b0:b1:b2:b3:rest) =
    supportsTLS False (supernet || abba, hypernet) (b1 : b2 : b3 : rest)
  where
    abba = b0 == b3 && b0 /= b1 && b1 == b2
supportsTLS True (supernet, hypernet) (b0:b1:b2:b3:rest) =
    supportsTLS True (supernet, hypernet && notAbba) (b1 : b2 : b3 : rest)
  where
    notAbba = b0 /= b3 || b0 == b1 || b1 /= b2

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
