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
import Lib (exit, pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

type Pair a = (a, a)

desiredChips :: Pair Int
desiredChips = (17, 61)

solve :: String -> String
solve input = pairToStr (first, second)
  where
    first = findProcessorBot desiredChips Map.empty instructions
    second =
        (Map.foldlWithKey
             (\p k v ->
                  p *
                  if k < 3
                      then head v
                      else 1)
             1 .
         computefinalOutputs Map.empty)
            instructions
    instructions =
        (foldl makeInstructionsAndState (Map.empty, Map.empty) . lines) input

computefinalOutputs ::
       Map Int [Int]
    -> (Map Int (Pair (Bool, Int)), Map Int [Int])
    -> Map Int [Int]
computefinalOutputs outputs (instructions, state) =
    if b < 0
        then o
        else computefinalOutputs o (instructions, s)
  where
    (b, _, (o, s)) = doInstruction desiredChips instructions (outputs, state)

findProcessorBot ::
       Pair Int
    -> Map Int [Int]
    -> (Map Int (Pair (Bool, Int)), Map Int [Int])
    -> Int
findProcessorBot dc outputs (instructions, state) =
    if c == dc
        then botId
        else findProcessorBot dc newO (instructions, newS)
  where
    (botId, c, (newO, newS)) = doInstruction dc instructions (outputs, state)

doInstruction ::
       Pair Int
    -> Map Int (Pair (Bool, Int))
    -> Pair (Map Int [Int])
    -> (Int, Pair Int, Pair (Map Int [Int]))
doInstruction dc instructions (outputs, state) = (botId, c, (o, s))
  where
    (botId, c, o, s) =
        case botState of
            ((b, [c0, c1]):_) -> (b, (c0, c1), o1, s1)
                where (o1, s1) =
                          if k1
                              then (Map.insertWith (++) t1 [c1] o0, s0)
                              else ( o0
                                   , Map.insertWith
                                         (\new old -> sort (old ++ new))
                                         t1
                                         [c1]
                                         s0)
                      (o0, s0) =
                          if k0
                              then ( Map.insertWith (++) t0 [c0] outputs
                                   , newState)
                              else ( outputs
                                   , Map.insertWith
                                         (\new old -> sort (old ++ new))
                                         t0
                                         [c0]
                                         newState)
                      Just ((k0, t0), (k1, t1)) = Map.lookup botId instructions
                      newState = Map.delete botId state
            [] -> (-1, dc, outputs, state)
    botState = (filter (\(_, chips) -> length chips == 2) . Map.toList) state

makeInstructionsAndState ::
       (Map Int (Pair (Bool, Int)), Map Int [Int])
    -> String
    -> (Map Int (Pair (Bool, Int)), Map Int [Int])
makeInstructionsAndState (instructions, state) command =
    case kind of
        "value" -> (instructions, addStateValue state rest)
        "bot" -> (addInstruction instructions rest, state)
  where
    (kind:rest) = words command

addInstruction ::
       Map Int (Pair (Bool, Int)) -> [String] -> Map Int (Pair (Bool, Int))
addInstruction instructions [botStr, _, _, _, k0Str, t0Str, _, _, _, k1Str, t1Str] =
    Map.insert botId ((k0, t0), (k1, t1)) instructions
  where
    botId = read botStr
    k0 = k0Str == "output"
    t0 = read t0Str
    k1 = k1Str == "output"
    t1 = read t1Str

addStateValue :: Map Int [Int] -> [String] -> Map Int [Int]
addStateValue state [chipStr, _, _, _, botStr] =
    Map.insertWith (\new old -> sort (old ++ new)) botId [chipId] state
  where
    botId = read botStr :: Int
    chipId = read chipStr :: Int

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
