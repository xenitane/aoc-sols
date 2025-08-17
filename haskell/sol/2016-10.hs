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
        (Map.foldrWithKey
             (\k v ->
                  (* if k < 3
                         then (head . Set.elems) v
                         else 1))
             1 .
         computefinalOutputs Map.empty)
            instructionsAndState
    instructionsAndState =
        (foldl makeInstructionsAndState (Map.empty, Map.empty) . lines) input

computefinalOutputs ::
       Map Int (Set Int)
    -> (Map Int (Pair (Bool, Int)), Map Int (Set Int))
    -> Map Int (Set Int)
computefinalOutputs outputs (instructions, state) =
    if botId < 0
        then outputs'
        else computefinalOutputs outputs' (instructions, state')
  where
    (botId, _, (outputs', state')) = doInstruction instructions (outputs, state)

findProcessorBot ::
       Pair Int
    -> Map Int (Set Int)
    -> (Map Int (Pair (Bool, Int)), Map Int (Set Int))
    -> Int
findProcessorBot dc outputs (instructions, state) =
    if c == dc
        then botId
        else findProcessorBot dc outputs' (instructions, state')
  where
    (botId, c, (outputs', state')) = doInstruction instructions (outputs, state)

doInstruction ::
       Map Int (Pair (Bool, Int))
    -> Pair (Map Int (Set Int))
    -> (Int, Pair Int, Pair (Map Int (Set Int)))
doInstruction instructions (outputs, state) =
    case (Map.toList . Map.filter ((== 2) . Set.size)) state of
        ((botId, chips):_) -> (botId, (chip, chip'), (outputs', state'))
            where (outputs', state') =
                      (moveChip kindTarget' chip' . moveChip kindTarget chip)
                          (outputs, Map.delete botId state)
                  Just (kindTarget, kindTarget') = Map.lookup botId instructions
                  [chip, chip'] = Set.elems chips
        [] -> (-1, (0, 0), (outputs, state))

moveChip ::
       (Bool, Int)
    -> Int
    -> Pair (Map Int (Set Int))
    -> Pair (Map Int (Set Int))
moveChip (kind, target) chip (outputs, state) =
    if kind
        then (addChip outputs, state)
        else (outputs, addChip state)
  where
    addChip = Map.insertWith Set.union target (Set.singleton chip)

makeInstructionsAndState ::
       (Map Int (Pair (Bool, Int)), Map Int (Set Int))
    -> String
    -> (Map Int (Pair (Bool, Int)), Map Int (Set Int))
makeInstructionsAndState (instructions, state) command =
    case kind of
        "value" -> (instructions, addStateValue state rest)
        "bot" -> (addInstruction instructions rest, state)
  where
    (kind:rest) = words command

addInstruction ::
       Map Int (Pair (Bool, Int)) -> [String] -> Map Int (Pair (Bool, Int))
addInstruction instructions [botStr, _, _, _, kStr, tStr, _, _, _, kStr', tStr'] =
    Map.insert botId ((k, t), (k', t')) instructions
  where
    botId = read botStr
    k = kStr == "output"
    t = read tStr
    k' = kStr' == "output"
    t' = read tStr'

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
