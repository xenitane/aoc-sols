{-# LANGUAGE CPP #-}

import Data.List (intercalate)
import Data.List.Split (condense, dropBlanks, dropDelims, oneOf, split)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (both)
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
            (Set.empty, Set.singleton (0, floors0))
            (Set.empty, Set.singleton (length finalFloors0 - 1, finalFloors0))
    second =
        minimumSteps
            0
            (Set.empty, Set.singleton (0, floors1))
            (Set.empty, Set.singleton (length finalFloors1 - 1, finalFloors1))
    (finalFloors0, finalFloors1) =
        let mergeFloors floors =
                replicate (length floors - 1) (Set.empty, Set.empty) ++
                [foldl (boths Set.union) (Set.empty, Set.empty) floors]
         in both mergeFloors (floors0, floors1)
    floors1 =
        (foldl (flip addToFloor) (head floors0) .
         concatMap (\x -> map (Just . (, x)) [True, False]))
            [Map.size elemIds, Map.size elemIds + 1] :
        drop 1 floors0
    (floors0, elemIds) =
        let splitter =
                (split . dropBlanks . condense . dropDelims . oneOf) " .,"
            accFunc a = makeFloors a . splitter
         in (foldl accFunc ([], Map.empty) . lines) input

boths :: (a -> b -> c) -> Pair a -> Pair b -> Pair c
boths f (a, a') (b, b') = (f a b, f a' b')

minimumSteps ::
       Int
    -> (Set String, Set (Int, [Pair (Set Int)]))
    -> (Set String, Set (Int, [Pair (Set Int)]))
    -> Int
minimumSteps depth (prev, curr) (prev', curr')
    | null curr || null curr' = infinity
    | (not . null . Set.intersection curr) curr' = depth
    | (not . null . Set.intersection prev' . Set.map buildingStateToStr) curr =
        depth - 1
    | otherwise =
        minimumSteps
            (depth + 2)
            ( (Set.union prev . Set.map buildingStateToStr) curr
            , nextStatesFromSet prev curr)
            ( (Set.union prev' . Set.map buildingStateToStr) curr'
            , nextStatesFromSet prev' curr')

buildingStateToStr :: (Int, [Pair (Set Int)]) -> String
buildingStateToStr (floor, materials) =
    let f = intercalate "," . map show . Set.elems
        g (a, b) = f a ++ ":" ++ f b
     in show floor ++ "|" ++ (intercalate ";" . map g) materials

nextStatesFromSet ::
       Set String -> Set (Int, [Pair (Set Int)]) -> Set (Int, [Pair (Set Int)])
nextStatesFromSet prevStates = Set.foldl (nextStatesAll prevStates) Set.empty

nextStatesAll ::
       Set String
    -> Set (Int, [Pair (Set Int)])
    -> (Int, [Pair (Set Int)])
    -> Set (Int, [Pair (Set Int)])
nextStatesAll prevStates collectedStates (curFloor, materialState) =
    let currFloorState = materialState !! curFloor
        takables =
            [Nothing] ++
            (map (Just . (True, )) . Set.toList . fst) currFloorState ++
            (map (Just . (False, )) . Set.toList . snd) currFloorState
     in (Set.union collectedStates .
         foldl
             (nextStateFromFloorAndTakable prevStates curFloor materialState)
             Set.empty .
         concatMap (\(i, x) -> map (x, ) (drop i takables)) . zip [1 ..])
            takables

nextStateFromFloorAndTakable ::
       Set String
    -> Int
    -> [Pair (Set Int)]
    -> Set (Int, [Pair (Set Int)])
    -> Pair (Maybe (Bool, Int))
    -> Set (Int, [Pair (Set Int)])
nextStateFromFloorAndTakable prevStates currFloor materialState collectedNextStates (t, t') =
    if (not . isFloorValid) materialState'
        then collectedNextStates
        else (Set.union collectedNextStates . Set.fromList)
                 (materialStateDown ++ materialStateUp)
  where
    materialStateDown =
        let msd = setAt (currFloor - 1) materialsDown nms
            materialsDown =
                (addToFloor t' . addToFloor t . (!! (currFloor - 1))) nms
         in [ (currFloor - 1, msd)
            | currFloor > 0 &&
                  isFloorValid materialsDown &&
                  (not . flip Set.member prevStates . buildingStateToStr)
                      (currFloor - 1, msd)
            ]
    materialStateUp =
        let msu = setAt (currFloor + 1) materialsUp nms
            materialsUp =
                (addToFloor t' . addToFloor t . (!! (currFloor + 1))) nms
         in [ (currFloor + 1, msu)
            | currFloor + 1 < length nms &&
                  isFloorValid materialsUp &&
                  (not . flip Set.member prevStates . buildingStateToStr)
                      (currFloor + 1, msu)
            ]
    nms = setAt currFloor materialState' materialState
    materialState' =
        (deleteFromFloor t' . deleteFromFloor t . (!! currFloor)) materialState

setAt :: Int -> a -> [a] -> [a]
setAt idx val arr = take idx arr ++ [val] ++ drop (idx + 1) arr

addToFloor :: Ord a => Maybe (Bool, a) -> Pair (Set a) -> Pair (Set a)
addToFloor = alterFloor Set.insert

deleteFromFloor :: Ord a => Maybe (Bool, a) -> Pair (Set a) -> Pair (Set a)
deleteFromFloor = alterFloor Set.delete

alterFloor ::
       Ord a
    => (a -> Set a -> Set a)
    -> Maybe (Bool, a)
    -> Pair (Set a)
    -> Pair (Set a)
alterFloor _ Nothing floorState = floorState
alterFloor f (Just (isGen, a)) (gen, mc) =
    if isGen
        then (f a gen, mc)
        else (gen, f a mc)

isFloorValid :: Ord a => Pair (Set a) -> Bool
isFloorValid (generators, microchips) =
    Set.null generators || Set.isSubsetOf microchips generators

makeFloors ::
       ([Pair (Set Int)], Map String Int)
    -> [String]
    -> ([Pair (Set Int)], Map String Int)
makeFloors (arr, elemIds) toks =
    let (floor, elemIds') = makeFloor elemIds toks
     in (arr ++ [floor], elemIds')

makeFloor :: Map String Int -> [String] -> (Pair (Set Int), Map String Int)
makeFloor elemIds (_:_:_:_:"nothing":_) = ((Set.empty, Set.empty), elemIds)
makeFloor elemIds (_:_:_:_:toks) = addGCs elemIds toks

addGCs :: Map String Int -> [String] -> (Pair (Set Int), Map String Int)
addGCs elemIds [] = ((Set.empty, Set.empty), elemIds)
addGCs elemIds ("and":toks) = addGCs elemIds toks
addGCs elemIds (_:elem:"generator":toks) =
    (addToFloor (Just (True, elemId)) floorState, elemIds')
  where
    (elemId, elemIds') =
        case Map.lookup elem elemIdsTemp of
            Just x -> (x, elemIdsTemp)
            Nothing ->
                ( Map.size elemIdsTemp
                , Map.insert elem (Map.size elemIdsTemp) elemIdsTemp)
    (floorState, elemIdsTemp) = addGCs elemIds toks
addGCs elemIds (_:elem:"microchip":toks) =
    (addToFloor (Just (False, elemId)) floorState, elemIds')
  where
    (elemId, elemIds') =
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
