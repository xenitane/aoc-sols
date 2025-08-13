{-# LANGUAGE CPP #-}

import Data.Char (chr, ord)
import Data.List (elemIndex)
import Data.Map (Map, lookup)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import Data.Tuple.Extra (both)
import Lib (exit, pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

moveId :: Char -> Int
moveId 'L' = 0
moveId 'U' = 1
moveId 'R' = 2
moveId 'D' = 3

move1map =
    Map.fromList
        [ ('1', "1124")
        , ('2', "1235")
        , ('3', "2336")
        , ('4', "4157")
        , ('5', "4268")
        , ('6', "5369")
        , ('7', "7487")
        , ('8', "7598")
        , ('9', "8699")
        ]

move2map =
    Map.fromList
        [ ('1', "1113")
        , ('2', "2236")
        , ('3', "2147")
        , ('4', "3448")
        , ('5', "5565")
        , ('6', "527A")
        , ('7', "638B")
        , ('8', "749C")
        , ('9', "8999")
        , ('A', "A6BA")
        , ('B', "A7CD")
        , ('C', "B8CC")
        , ('D', "DBDD")
        ]

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) =
        (foldl (reposition (move1map, move2map)) ("", "") . lines) input

type Pair a = (a, a)

reposition :: Pair (Map Char String) -> Pair String -> String -> Pair String
reposition moveMaps ("", "") =
    both (: []) . moveFromAccToCode moveMaps ('5', '5')
reposition moveMaps s =
    boths (\a b -> a ++ [b]) s . moveFromAccToCode moveMaps (both last s)

moveFromAccToCode :: Pair (Map Char String) -> Pair Char -> String -> Pair Char
moveFromAccToCode _ c "" = c
moveFromAccToCode moveMaps c (h:r) =
    moveFromAccToCode moveMaps (next moveMaps (moveId h) c) r

next :: Pair (Map Char String) -> Int -> Pair Char -> Pair Char
next mms idx keys =
    both
        (last . take (idx + 1) . Maybe.fromMaybe "0000")
        (boths Map.lookup keys mms)

boths :: (a -> b -> c) -> Pair a -> Pair b -> Pair c
boths f (a0, a1) (b0, b1) = (f a0 b0, f a1 b1)

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
