{-# LANGUAGE CPP #-}

import Data.List (sort)
import Data.Map
    ( Map
    , delete
    , empty
    , foldlWithKey
    , insert
    , insertWith
    , lookup
    , toList
    )
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lib (exit, pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

type Pair a = (a, a)

desiredChips :: Pair Int
desiredChips = (17, 61)

solve :: String -> String
solve input = pairToStr (first, second)
  where
    first = findProcessorBot desiredChips Map.empty instructionsAndState
    second =
        let mulFunc k v =
                let vv = (head . ([1 | k >= 3] ++) . Set.elems) v
                 in (* vv)
         in (Map.foldrWithKey mulFunc 1 . computefinalOutputs Map.empty)
                instructionsAndState
    instructionsAndState =
        (foldl makeInstructionsAndState (Map.empty, Map.empty) . lines) input

computefinalOutputs ::
       Map Int (Set Int)
    -> (Map Int (Pair (Bool, Int)), Map Int (Set Int))
    -> Map Int (Set Int)
computefinalOutputs outputs (instructions, state) =
    let (botId, _, (outputs', state')) =
            doInstruction instructions (outputs, state)
     in if botId < 0
            then outputs'
            else computefinalOutputs outputs' (instructions, state')

findProcessorBot ::
       Pair Int
    -> Map Int (Set Int)
    -> (Map Int (Pair (Bool, Int)), Map Int (Set Int))
    -> Int
findProcessorBot dc outputs (instructions, state) =
    let (botId, c, (outputs', state')) =
            doInstruction instructions (outputs, state)
     in if c == dc
            then botId
            else findProcessorBot dc outputs' (instructions, state')

doInstruction ::
       Map Int (Pair (Bool, Int))
    -> Pair (Map Int (Set Int))
    -> (Int, Pair Int, Pair (Map Int (Set Int)))
doInstruction instructions (outputs, state) =
    case (Map.toList . Map.filter ((== 2) . Set.size)) state of
        ((botId, chips):_) ->
            let [chip, chip'] = Set.elems chips
                Just (kindTarget, kindTarget') = Map.lookup botId instructions
                (outputs', state') =
                    (moveChip kindTarget' chip' . moveChip kindTarget chip)
                        (outputs, Map.delete botId state)
             in (botId, (chip, chip'), (outputs', state'))
        [] -> (-1, (0, 0), (outputs, state))

moveChip ::
       (Bool, Int)
    -> Int
    -> Pair (Map Int (Set Int))
    -> Pair (Map Int (Set Int))
moveChip (kind, target) chip (outputs, state) =
    let addChip = Map.insertWith Set.union target (Set.singleton chip)
     in if kind
            then (addChip outputs, state)
            else (outputs, addChip state)

makeInstructionsAndState ::
       (Map Int (Pair (Bool, Int)), Map Int (Set Int))
    -> String
    -> (Map Int (Pair (Bool, Int)), Map Int (Set Int))
makeInstructionsAndState (instructions, state) command =
    case words command of
        ("value":rest) -> (instructions, addStateValue state rest)
        ("bot":rest) -> (addInstruction instructions rest, state)

addInstruction ::
       Map Int (Pair (Bool, Int)) -> [String] -> Map Int (Pair (Bool, Int))
addInstruction instructions [botStr, _, _, _, kStr, tStr, _, _, _, kStr', tStr'] =
    let botId = read botStr
        k = kStr == "output"
        t = read tStr
        k' = kStr' == "output"
        t' = read tStr'
     in Map.insert botId ((k, t), (k', t')) instructions

addStateValue :: Map Int (Set Int) -> [String] -> Map Int (Set Int)
addStateValue state [chipStr, _, _, _, botStr] =
    Map.insertWith
        Set.union
        (read botStr)
        ((Set.singleton . read) chipStr)
        state

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
