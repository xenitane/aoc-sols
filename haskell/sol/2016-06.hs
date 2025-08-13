{-# LANGUAGE CPP #-}

import Data.List (sortBy)
import Data.Map (Map, empty, toList)
import qualified Data.Map as Map
import Lib (exit, pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

type Pair a = (a, a)

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) =
        (foldl getMaxMin ("", "") .
         Map.elems . foldl mergeLines Map.empty . lines)
            input

getMaxMin :: Pair String -> Map Char Int -> Pair String
getMaxMin (a, b) indFrqMap = (a ++ [x], b ++ [y])
  where
    (x, _) = last items
    (y, _) = head items
    items =
        (sortBy (\(b0, f0) (b1, f1) -> compare (f0, b0) (f1, b1)) . Map.toList)
            indFrqMap

mergeLines :: Map Int (Map Char Int) -> String -> Map Int (Map Char Int)
mergeLines frqMap = addChar frqMap 0

addChar :: Map Int (Map Char Int) -> Int -> String -> Map Int (Map Char Int)
addChar frqMap _ "" = frqMap
addChar frqMap idx (c:rest) = addChar newMap (idx + 1) rest
  where
    newMap = Map.insert idx newVal frqMap
    newVal =
        case Map.lookup idx frqMap of
            Nothing -> Map.fromList [(c, 1)]
            Just imap -> Map.insertWith (+) c 1 imap

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
