{-# LANGUAGE CPP #-}

import Data.List.Split (condense, dropBlanks, dropDelims, oneOf, split)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lib (exit, pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

type Pair a = (a, a)

infinity :: Int
infinity = 9223372036854775807

solve :: String -> String
solve input = pairToStr (first, second)
  where
    first =
        minimumSteps
            0
            (Set.empty, Set.fromList [(0, floors0)])
            (Set.empty, Set.fromList [(length finalFloors0 - 1, finalFloors0)])
    second =
        minimumSteps
            0
            (Set.empty, Set.fromList [(0, floors1)])
            (Set.empty, Set.fromList [(length finalFloors1 - 1, finalFloors1)])
    finalFloors1 =
        replicate (length floors1 - 1) (Set.empty, Set.empty) ++
        [ foldl
              (\(p0, p1) (s0, s1) -> (Set.union p0 s0, Set.union p1 s1))
              (Set.empty, Set.empty)
              floors1
        ]
    floors1 =
        foldl
            (flip addToFloor)
            (head floors0)
            [ Just (True, Map.size elemIds)
            , Just (False, Map.size elemIds)
            , Just (True, Map.size elemIds + 1)
            , Just (False, Map.size elemIds + 1)
            ] :
        drop 1 floors0
    finalFloors0 =
        replicate (length floors0 - 1) (Set.empty, Set.empty) ++
        [ foldl
              (\(p0, p1) (s0, s1) -> (Set.union p0 s0, Set.union p1 s1))
              (Set.empty, Set.empty)
              floors0
        ]
    (floors0, elemIds) =
        (foldl
             (\a b ->
                  (makeFloors a .
                   (split . dropBlanks . condense . dropDelims . oneOf) " .,")
                      b)
             ([], Map.empty) .
         lines)
            input

minimumSteps ::
       Int
    -> Pair (Set (Int, [Pair (Set Int)]))
    -> Pair (Set (Int, [Pair (Set Int)]))
    -> Int
minimumSteps depth (prev0, curr0) (prev1, curr1)
    | null curr0 || null curr1 = infinity
    | (not . null) (Set.intersection curr0 curr1) = depth
    | (not . null) (Set.intersection curr0 prev1) = depth - 1
    | (not . null) (Set.intersection curr1 prev0) = depth - 1
    | otherwise =
        minimumSteps
            (depth + 2)
            (Set.union prev0 curr0, nextStatesFromSet prev0 curr0)
            (Set.union prev1 curr1, nextStatesFromSet prev1 curr1)

nextStatesFromSet ::
       Set (Int, [Pair (Set Int)])
    -> Set (Int, [Pair (Set Int)])
    -> Set (Int, [Pair (Set Int)])
nextStatesFromSet prevStates = Set.foldl (nextStatesAll prevStates) Set.empty

nextStatesAll ::
       Set (Int, [Pair (Set Int)])
    -> Set (Int, [Pair (Set Int)])
    -> (Int, [Pair (Set Int)])
    -> Set (Int, [Pair (Set Int)])
nextStatesAll seenStates collectedStates (curFloor, materialState) =
    (Set.union collectedStates .
     foldl
         (nextStateFromFloorAndTakable seenStates curFloor materialState)
         Set.empty .
     concatMap (\(i, x) -> map (x, ) (drop i takables)) . zip [1 ..])
        takables
  where
    takables =
        [Nothing] ++
        (map (\x -> Just (True, x)) . Set.toList . fst) currFloorState ++
        (map (\x -> Just (False, x)) . Set.toList . snd) currFloorState
    currFloorState = materialState !! curFloor

nextStateFromFloorAndTakable ::
       Set (Int, [Pair (Set Int)])
    -> Int
    -> [Pair (Set Int)]
    -> Set (Int, [Pair (Set Int)])
    -> Pair (Maybe (Bool, Int))
    -> Set (Int, [Pair (Set Int)])
nextStateFromFloorAndTakable seenStates currFloor materialState collectedNextStates (t0, t1) =
    if (not . isFloorValid) newMaterialsNow
        then collectedNextStates
        else (Set.union collectedNextStates . Set.fromList)
                 (materialStateDown ++ materialStateUp)
  where
    materialStateDown
        | currFloor == 0 = []
        | isFloorValid materialsDown &&
              not (Set.member (currFloor - 1, msd) seenStates) =
            [(currFloor - 1, msd)]
        | otherwise = []
      where
        msd = setAt (currFloor - 1) materialsDown nms
        materialsDown =
            (addToFloor t1 . addToFloor t0 . (!! (currFloor - 1))) nms
    materialStateUp
        | currFloor + 1 >= length nms = []
        | isFloorValid materialsUp &&
              not (Set.member (currFloor + 1, msu) seenStates) =
            [(currFloor + 1, msu)]
        | otherwise = []
      where
        msu = setAt (currFloor + 1) materialsUp nms
        materialsUp = (addToFloor t1 . addToFloor t0 . (!! (currFloor + 1))) nms
    nms = setAt currFloor newMaterialsNow materialState
    newMaterialsNow =
        (deleteFromFloor t1 . deleteFromFloor t0 . (!! currFloor)) materialState

setAt :: Int -> a -> [a] -> [a]
setAt idx val arr = take idx arr ++ [val] ++ drop (idx + 1) arr

addToFloor :: Ord a => Maybe (Bool, a) -> Pair (Set a) -> Pair (Set a)
addToFloor Nothing floorState = floorState
addToFloor (Just (isGen, name)) (gen, mc) =
    if isGen
        then (Set.insert name gen, mc)
        else (gen, Set.insert name mc)

deleteFromFloor :: Ord a => Maybe (Bool, a) -> Pair (Set a) -> Pair (Set a)
deleteFromFloor Nothing floorState = floorState
deleteFromFloor (Just (isGen, name)) (gen, mc) =
    if isGen
        then (Set.delete name gen, mc)
        else (gen, Set.delete name mc)

isFloorValid :: Ord a => Pair (Set a) -> Bool
isFloorValid (a, b) = Set.null a || Set.isSubsetOf b a

makeFloors ::
       ([Pair (Set Int)], Map String Int)
    -> [String]
    -> ([Pair (Set Int)], Map String Int)
makeFloors (arr, elemIds) toks = (arr ++ [floor], newElemIds)
  where
    (floor, newElemIds) = makeFloor elemIds toks

makeFloor :: Map String Int -> [String] -> (Pair (Set Int), Map String Int)
makeFloor elemIds (_:_:_:_:"nothing":_) = ((Set.empty, Set.empty), elemIds)
makeFloor elemIds (_:_:_:_:toks) = addGCs elemIds toks

addGCs :: Map String Int -> [String] -> (Pair (Set Int), Map String Int)
addGCs elemIds [] = ((Set.empty, Set.empty), elemIds)
addGCs elemIds ("and":toks) = addGCs elemIds toks
addGCs elemIds (_:elem:"generator":toks) =
    (addToFloor (Just (True, elemId)) floorState, newElemIds)
  where
    (elemId, newElemIds) =
        case Map.lookup elem elemIdsTemp of
            Just x -> (x, elemIdsTemp)
            Nothing ->
                ( Map.size elemIdsTemp
                , Map.insert elem (Map.size elemIdsTemp) elemIdsTemp)
    (floorState, elemIdsTemp) = addGCs elemIds toks
addGCs elemIds (_:elem:"microchip":toks) =
    (addToFloor (Just (False, elemId)) floorState, newElemIds)
  where
    (elemId, newElemIds) =
        case Map.lookup elemName elemIdsTemp of
            Just x -> (x, elemIdsTemp)
            Nothing ->
                ( Map.size elemIdsTemp
                , Map.insert elemName (Map.size elemIdsTemp) elemIdsTemp)
    (floorState, elemIdsTemp) = addGCs elemIds toks
    elemName = takeWhile (/= '-') elem

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
