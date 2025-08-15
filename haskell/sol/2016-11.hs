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
infinity = 1000

solve :: String -> String
solve input = pairToStr (first, second)
  where
    first = (minimumSteps 0 Set.empty . Set.fromList) [(0, floors)]
    second =
        (minimumSteps 0 Set.empty . Set.fromList)
            [ ( 0
              , foldl
                    (flip addToFloor)
                    (head floors)
                    [ Just (True, "elerium")
                    , Just (False, "elerium")
                    , Just (True, "dilithium")
                    , Just (False, "dilithium")
                    ] :
                drop 1 floors)
            ]
    floors =
        (map (makeFloor .
              (split . dropBlanks . condense . dropDelims . oneOf) " .,") .
         lines)
            input

minimumSteps ::
       Int
    -> Set (Int, [Pair (Set String)])
    -> Set (Int, [Pair (Set String)])
    -> Int
minimumSteps depth seenStates curStates
    | depth == infinity || null curStates = infinity
    | otherwise =
        if (not . null) curStates &&
           any
               (\(x, arr) ->
                    x + 1 == length arr &&
                    (all (\(a, b) -> Set.null a && Set.null b) .
                     drop 1 . reverse)
                        arr)
               curStates
            then depth
            else (minimumSteps (depth + 1) (Set.union seenStates curStates) .
                  foldl (nextStatesAll seenStates) Set.empty . Set.toList)
                     curStates

nextStatesAll ::
       Set (Int, [Pair (Set String)])
    -> Set (Int, [Pair (Set String)])
    -> (Int, [Pair (Set String)])
    -> Set (Int, [Pair (Set String)])
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
       Set (Int, [Pair (Set String)])
    -> Int
    -> [Pair (Set String)]
    -> Set (Int, [Pair (Set String)])
    -> Pair (Maybe (Bool, String))
    -> Set (Int, [Pair (Set String)])
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

addToFloor :: Maybe (Bool, String) -> Pair (Set String) -> Pair (Set String)
addToFloor Nothing floorState = floorState
addToFloor (Just (isGen, name)) (gen, mc) =
    if isGen
        then (Set.insert name gen, mc)
        else (gen, Set.insert name mc)

deleteFromFloor ::
       Maybe (Bool, String) -> Pair (Set String) -> Pair (Set String)
deleteFromFloor Nothing floorState = floorState
deleteFromFloor (Just (isGen, name)) (gen, mc) =
    if isGen
        then (Set.delete name gen, mc)
        else (gen, Set.delete name mc)

isFloorValid :: Pair (Set String) -> Bool
isFloorValid (a, b) = Set.null a || Set.isSubsetOf b a

makeFloor :: [String] -> Pair (Set String)
makeFloor (_:_:_:_:"nothing":_) = (Set.empty, Set.empty)
makeFloor (_:_:_:_:toks) = addGCs toks

addGCs :: [String] -> Pair (Set String)
addGCs [] = (Set.empty, Set.empty)
addGCs ("and":toks) = addGCs toks
addGCs (_:elem:"generator":toks) = (Set.insert elem generators, microchips)
  where
    (generators, microchips) = addGCs toks
addGCs (_:elem:"microchip":toks) = (generators, Set.insert elemName microchips)
  where
    elemName = takeWhile (/= '-') elem
    (generators, microchips) = addGCs toks

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
