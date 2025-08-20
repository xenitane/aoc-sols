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

import Data.Bits (popCount)
import Data.Set (Set)
import qualified Data.Set as Set

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
        targetPoint |> Set.singleton |> (Set.empty, ) |>
        minimumSteps shift 0 (Set.empty, Set.singleton startPoint)
    second =
        startPoint |> Set.singleton |>
        countReachableInSteps shift numItr Set.empty
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
    | Set.intersection curr0 curr1 |> null |> not = depth
    | Set.intersection curr0 prev1 |> null |> not = depth - 1
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
    let surrounds = [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)]
        shouldStep p =
            isPointOpenWithShift shift p && not (Set.member p seenPoints)
     in surrounds |> filter shouldStep |> Set.fromList |>
        Set.union collectedPoints

isPointOpenWithShift :: Int -> Point -> Bool
isPointOpenWithShift shift (x, y)
    | x >= 0 && y >= 0 =
        (shift + (x + y) * (x + y) + 3 * x + y) |> popCount |> even
    | otherwise = False

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
            then pStr' "test passed\n"
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
