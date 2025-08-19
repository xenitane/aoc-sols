{-# LANGUAGE CPP #-}

import Lib
    ( Pair
    , ($*)
    , (*$)
    , (*$*)
    , (=:>)
    , (|>)
    , block
    , boths
    , exit
    , logId
    , pStr
    , pStr'
    , pairToStr
    , sLogId
    , safeReadFile
    , setAt
    , trimTrailing
    )

import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

desiredChips :: Pair Int
desiredChips = (17, 61)

solve :: String -> String
solve input = pairToStr (first, second)
  where
    first = findProcessorBot desiredChips Map.empty instructionsAndState
    second =
        let mulFunc k v =
                let vv = v |> Set.elems |> ([1 | k >= 3] ++) |> head
                 in (* vv)
         in instructionsAndState |> computefinalOutputs Map.empty |>
            Map.foldrWithKey mulFunc 1
    instructionsAndState =
        input |> lines |> foldl makeInstructionsAndState (Map.empty, Map.empty)

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
    case state |> Map.filter (Set.size =:> (== 2)) |> Map.toList of
        ((botId, chips):_) ->
            let [chip, chip'] = Set.elems chips
                Just (kindTarget, kindTarget') = Map.lookup botId instructions
                (outputs', state') =
                    (outputs, Map.delete botId state) |>
                    moveChip kindTarget chip |>
                    moveChip kindTarget' chip'
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
        (chipStr |> read |> Set.singleton)
        state

main :: IO ()
#if defined YEAR && defined DAY
suff :: FilePath
suff = "/" ++ YEAR ++ "-" ++ DAY ++ ".txt"
#if !defined TEST_MODE
main = do
    input <- safeReadFile $ "../inputs" ++ suff
    input |> solve |> pStr
#else
main = do
    input <- safeReadFile $ "../test_inputs" ++ suff
    expected' <- safeReadFile $ "../test_outputs" ++ suff
    let actual = input |> solve |> trimTrailing
        expected = trimTrailing expected'
     in if actual == expected
            then pStr' "test passes\n"
            else do
                pStr' "test failed\n"
                block "Expected" expected
                block "Actual" actual
                exit 1
#endif
#else
main = do
    pStr' "essential variables not defined"
    exit 1
#endif
