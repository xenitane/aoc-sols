{-# LANGUAGE CPP #-}

import Data.Bits (popCount)
import Data.Set (Set)
import qualified Data.Set as Set
import Lib (exit, pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

type Pair a = (a, a)

type Point = Pair Int

infinity :: Int
infinity = 9223372036854775807

startPoint :: Point
startPoint = (1, 1)

targetPoint :: Point
targetPoint = (31, 39)

numItr :: Int
numItr = 50

solve :: String -> String
solve input = pairToStr (first, second)
  where
    first =
        (minimumSteps shift 0 (Set.empty, Set.singleton startPoint) .
         (Set.empty, ) . Set.singleton)
            targetPoint
    second =
        (countReachableInSteps shift numItr Set.empty . Set.singleton)
            startPoint
    shift = read input

countReachableInSteps :: Int -> Int -> Set Point -> Set Point -> Int
countReachableInSteps shift stepsLeft reached currentlyOn
    | null currentlyOn = Set.size reached
    | 0 == stepsLeft = Set.size (Set.union reached currentlyOn)
    | otherwise =
        countReachableInSteps
            shift
            (stepsLeft - 1)
            (Set.union reached currentlyOn)
            (nextStatesFromSet shift reached currentlyOn)

minimumSteps :: Int -> Int -> Pair (Set Point) -> Pair (Set Point) -> Int
minimumSteps shift depth (prev0, curr0) (prev1, curr1)
    | null curr0 || null curr1 = infinity
    | (not . null) (Set.intersection curr0 curr1) = depth
    | (not . null) (Set.intersection curr0 prev1) = depth - 1
    | otherwise =
        minimumSteps
            shift
            (depth + 2)
            (Set.union prev0 curr0, nextStatesFromSet shift prev0 curr0)
            (Set.union prev1 curr1, nextStatesFromSet shift prev1 curr1)

nextStatesFromSet :: Int -> Set Point -> Set Point -> Set Point
nextStatesFromSet shift prevStates =
    Set.foldl (nextStatesAll shift prevStates) Set.empty

nextStatesAll :: Int -> Set Point -> Set Point -> Point -> Set Point
nextStatesAll shift seenPoints collectedPoints (x, y) =
    (Set.union collectedPoints .
     foldl
         (\s (nx, ny) ->
              if isPointOpenWithShift shift (nx, ny) &&
                 (not . Set.member (nx, ny)) seenPoints
                  then Set.insert (nx, ny) s
                  else s)
         Set.empty)
        [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)]

isPointOpenWithShift :: Int -> Point -> Bool
isPointOpenWithShift shift (x, y) =
    x >= 0 &&
    y >= 0 && (even . popCount) (shift + (x + y) * (x + y) + 3 * x + y)

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
