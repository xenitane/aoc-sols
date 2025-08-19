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

import Data.List (intercalate)
import Data.List.Split (condense, dropBlanks, dropDelims, oneOf, split)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

infinity :: Int
infinity = 9223372036854775807

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) =
        let mergeFloors floors =
                replicate (length floors - 1) (Set.empty, Set.empty) ++
                [foldl (boths Set.union) (Set.empty, Set.empty) floors]
            minimumSteps' x =
                let x' = mergeFloors x
                 in minimumSteps
                        0
                        (Set.empty, Set.singleton (0, x))
                        (Set.empty, Set.singleton (length x' - 1, x'))
         in minimumSteps' $* (floors0, floors1)
    floors1 =
        let mapFunc x = map ((, x) =:> Just) [True, False]
         in [Map.size elemIds, Map.size elemIds + 1] |> concatMap mapFunc |>
            foldl (flip addToFloor) (head floors0) |>
            (: drop 1 floors0)
    (floors0, elemIds) =
        let splitter =
                oneOf =:> dropDelims =:> condense =:> dropBlanks =:> split
            accFunc a = splitter " .," =:> makeFloors a
         in input |> lines |> foldl accFunc ([], Map.empty)

minimumSteps ::
       Int
    -> (Set String, Set (Int, [Pair (Set Int)]))
    -> (Set String, Set (Int, [Pair (Set Int)]))
    -> Int
minimumSteps depth (prev, curr) (prev', curr')
    | null curr || null curr' = infinity
    | curr' |> Set.intersection curr |> null |> not = depth
    | curr |> Set.map buildingStateToStr |> Set.intersection prev' |> null |>
          not = depth - 1
    | otherwise =
        minimumSteps
            (depth + 2)
            ( curr |> Set.map buildingStateToStr |> Set.union prev
            , nextStatesFromSet prev curr)
            ( curr' |> Set.map buildingStateToStr |> Set.union prev'
            , nextStatesFromSet prev' curr')

buildingStateToStr :: (Int, [Pair (Set Int)]) -> String
buildingStateToStr (floor, materials) =
    let f = Set.elems =:> map show =:> intercalate ","
        g (a, b) = f a ++ ":" ++ f b
     in materials |> map g |> intercalate ";" |> ("|" ++) |> (show floor ++)

nextStatesFromSet ::
       Set String -> Set (Int, [Pair (Set Int)]) -> Set (Int, [Pair (Set Int)])
nextStatesFromSet prevStates = Set.foldl (nextStatesAll prevStates) Set.empty

nextStatesAll ::
       Set String
    -> Set (Int, [Pair (Set Int)])
    -> (Int, [Pair (Set Int)])
    -> Set (Int, [Pair (Set Int)])
nextStatesAll prevStates collectedStates (currFloor, materialState) =
    let currFloorState = materialState !! currFloor
        takables =
            [ fst =:> Set.toList =:> map ((True, ) =:> Just)
            , snd =:> Set.toList =:> map ((False, ) =:> Just)
            ] |>
            map (currFloorState |>) |>
            intercalate [Nothing]
        mapFunc (i, x) = takables |> drop i |> map (x, )
     in takables |> zip [1 ..] |> concatMap mapFunc |>
        foldl
            (nextStateFromFloorAndTakable prevStates currFloor materialState)
            Set.empty |>
        Set.union collectedStates

nextStateFromFloorAndTakable ::
       Set String
    -> Int
    -> [Pair (Set Int)]
    -> Set (Int, [Pair (Set Int)])
    -> Pair (Maybe (Bool, Int))
    -> Set (Int, [Pair (Set Int)])
nextStateFromFloorAndTakable prevStates currFloor materialState collectedNextStates (t, t') =
    if materialState' |> isFloorValid |> not
        then collectedNextStates
        else (materialStateDown ++ materialStateUp) |> Set.fromList |>
             Set.union collectedNextStates
  where
    (materialStateDown, materialStateUp) =
        let materialStateGen x =
                let nms = setAt currFloor materialState' materialState
                    nextFloor = currFloor + x
                    nms' = setAt nextFloor materials' nms
                    materials' =
                        nms !! nextFloor |> addToFloor t |> addToFloor t'
                 in [ (nextFloor, nms')
                    | nextFloor >= 0 &&
                          nextFloor < length nms &&
                          isFloorValid materials' &&
                          ((nextFloor, nms') |> buildingStateToStr |>
                           flip Set.member prevStates |>
                           not)
                    ]
         in materialStateGen $* (-1, 1)
    materialState' =
        materialState !! currFloor |> deleteFromFloor t |> deleteFromFloor t'

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
