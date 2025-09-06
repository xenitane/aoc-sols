module Sol where

import Lib

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

solve :: String -> String
solve input = (first, second) |> pairToStr
  where
    (first, second) = poi |> Map.size |> findMinDist pairWiseDist
    pairWiseDist = poi |> Map.toList |> concatMap (bfs maze) |> Map.fromList
    (maze, poi) =
        input |> lines |> zip [0 ..] |> foldl makeMaze (Map.empty, Map.empty)

findMinDist :: Map (Pair Int) Int -> Int -> Pair Int
findMinDist pwd len = minimizeDist pwd (infinity, infinity) (Just perm)
  where
    perm = take (len - 1) [1 ..]

minimizeDist :: Map (Pair Int) Int -> Pair Int -> Maybe [Int] -> Pair Int
minimizeDist _ res Nothing = res
minimizeDist pwd res (Just perm) =
    perm |> nextPerm |> minimizeDist pwd (perm |> permDist pwd |> boths min res)

nextPerm :: [Int] -> Maybe [Int]
nextPerm str =
    if length str < 2 || isDec str
        then Nothing
        else Just str'
  where
    isDec [_] = True
    isDec (a:b:restN) = a > b && isDec (b : restN)
    str' = prefix ++ mergeInto el suffix
    mergeInto c suff =
        take 1 gts ++ filter (<= c) suff ++ [c] ++ (gts |> drop 1)
      where
        gts = suff |> filter (> c)
    el = str |> reverse |> (!! (k + 1))
    suffix = str |> reverse |> take (k + 1) |> sort
    prefix = str |> reverse |> drop (k + 2) |> reverse
    k = str |> reverse |> flpIdx 0
    flpIdx k [_] = k
    flpIdx k (a:b:rest) =
        if a > b
            then k
            else flpIdx (k + 1) (b : rest)

permDist :: Map (Pair Int) Int -> [Int] -> Pair Int
permDist pwd arr = (arr ++ [0]) |> ((id, ([0] ++)) *$) |> (permDistInt $*)
  where
    permDistInt [_] = 0
    permDistInt (a:b:rest) =
        let Just dd = Map.lookup (a, b) pwd
         in dd + permDistInt (b : rest)

bfs :: Map (Pair Int) Char -> (Char, Pair Int) -> [(Pair Int, Int)]
bfs maze (i, (x0, y0)) =
    bfsInt maze (Set.empty, Set.singleton (x0, y0)) 0 Map.empty
        |> map (\(j, k) -> (read @Int $* ([i], [j]), k))

bfsInt ::
       Map (Pair Int) Char
    -> Pair (Set (Pair Int))
    -> Int
    -> Map Char Int
    -> [(Char, Int)]
bfsInt maze (vis, toVis) d spcl
    | null toVis = Map.toList spcl
    | otherwise = bfsInt maze (vis', toVis') (d + 1) spcl'
  where
    vis' = Set.union vis toVis
    (toVis', spcl') = Set.foldl (genNext maze vis d) (Set.empty, spcl) toVis

genNext ::
       Map (Pair Int) Char
    -> Set (Pair Int)
    -> Int
    -> (Set (Pair Int), Map Char Int)
    -> Pair Int
    -> (Set (Pair Int), Map Char Int)
genNext maze vis d (nexts, spcl) (x, y) =
    [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] |> foldl ff (nexts, spcl)
  where
    ff (nexts', spcl') (x', y') =
        case Map.lookup (x', y') maze of
            Nothing -> (nexts', spcl')
            Just '#' -> (nexts', spcl')
            Just c ->
                let gg =
                        if c == '.'
                            then id
                            else Map.insert c (d + 1)
                 in if Set.member (x', y') vis
                        then (nexts', spcl')
                        else (Set.insert (x', y') nexts', gg spcl')

makeMaze ::
       (Map (Pair Int) Char, Map Char (Pair Int))
    -> (Int, String)
    -> (Map (Pair Int) Char, Map Char (Pair Int))
makeMaze (pathWall, posOfInt) (i, row) =
    let foldFunc (pw, poi) (j, c) = (Map.insert (i, j) c, poif) *$* (pw, poi)
          where
            poif =
                if c == '.' || c == '#'
                    then id
                    else Map.insert c (i, j)
     in row |> zip [0 ..] |> foldl foldFunc (pathWall, posOfInt)
