{-# LANGUAGE CPP #-}

import Data.Char (chr, ord)
import Data.List (elemIndex)
import Data.Map (Map, lookup)
import qualified Data.Map as Map
import Lib (pairToStr, safeReadFile, trimTrailing)
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

solve :: String -> (String, String)
solve input = (first, second)
  where
    first = foldl (reposition move1map) "" codes
    second = foldl (reposition move2map) "" codes
    codes = lines input
    reposition :: Map Char String -> String -> String -> String
    reposition moveMap "" code = [moveFromAccToCode moveMap '5' code]
    reposition moveMap s code = s ++ [moveFromAccToCode moveMap (last s) code]
    moveFromAccToCode :: Map Char String -> Char -> String -> Char
    moveFromAccToCode _ c "" = c
    moveFromAccToCode moveMap c (h:r) =
        moveFromAccToCode moveMap (next moveMap (moveId h) c) r
    next :: Map Char String -> Int -> Char -> Char
    next mp k c = x
      where
        x = last $ take (k + 1) str
        Just str = Data.Map.lookup c mp

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
