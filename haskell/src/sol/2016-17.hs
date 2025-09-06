module Sol where

import Lib

import Crypto.Hash
import qualified Data.ByteString.Char8 as BS
import Data.Char

solve :: String -> String
solve input = (first, second) |> pairToStr
  where
    first = findShortestPath (0, 0) (infinity, "") "" input |> snd
    second = findLongestPathLength (0, 0) 0 "" input

findShortestPath ::
       Pair Int -> (Int, String) -> String -> String -> (Int, String)
findShortestPath currLoc (minSteps, minPath) path prefix
    | (3, 3) == currLoc && length path < minSteps = (length path, path)
    | length path >= minSteps = (minSteps, minPath)
    | otherwise =
        let stepable p (s, c) = p ++ [s | ord c > 97]
            nextSteps =
                (prefix ++ path)
                    |> makeMD5Digest
                    |> zip "UDLR"
                    |> foldl stepable ""
            takeStep r s =
                let nextLoc = nxtLoc s currLoc
                 in if isOnGrid nextLoc
                        then findShortestPath nextLoc r (path ++ [s]) prefix
                        else r
         in foldl takeStep (minSteps, minPath) nextSteps

findLongestPathLength :: Pair Int -> Int -> String -> String -> Int
findLongestPathLength currLoc steps path prefix
    | (3, 3) == currLoc = path |> length |> max steps
    | otherwise =
        let stepable str (stepChar, hashChar) =
                str ++ [stepChar | ord hashChar > 97]
            nextSteps =
                (prefix ++ path)
                    |> makeMD5Digest
                    |> zip "UDLR"
                    |> foldl stepable ""
            takeStep currSteps stepChar = max currSteps r'
              where
                r' =
                    let nextLoc = nxtLoc stepChar currLoc
                     in if isOnGrid nextLoc
                            then findLongestPathLength
                                     nextLoc
                                     currSteps
                                     (path ++ [stepChar])
                                     prefix
                            else currSteps
         in foldl takeStep steps nextSteps

isOnGrid :: Pair Int -> Bool
isOnGrid (x, y) = x >= 0 && y >= 0 && x < 4 && y < 4

nxtLoc :: Char -> Pair Int -> Pair Int
nxtLoc 'U' (x, y) = (x, y - 1)
nxtLoc 'D' (x, y) = (x, y + 1)
nxtLoc 'L' (x, y) = (x - 1, y)
nxtLoc 'R' (x, y) = (x + 1, y)

makeMD5Digest :: String -> String
makeMD5Digest = BS.pack =:> hashWith MD5 =:> show =:> take 4
