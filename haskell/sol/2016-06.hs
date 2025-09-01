module Sol where

import Lib

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) =
        input
            |> lines
            |> foldl mergeLines Map.empty
            |> Map.elems
            |> foldl getMaxMin ("", "")

getMaxMin :: Pair String -> Map Char Int -> Pair String
getMaxMin (s, s') indFrqMap =
    let pFlip (a, b) = (b, a)
        comparator a a' = compare (pFlip a) (pFlip a')
        items = indFrqMap |> Map.toList |> sortBy comparator
        (c, _) = last items
        (c', _) = head items
     in (s ++ [c], s' ++ [c'])

mergeLines :: Map Int (Map Char Int) -> String -> Map Int (Map Char Int)
mergeLines frqMap = addChar frqMap 0

addChar :: Map Int (Map Char Int) -> Int -> String -> Map Int (Map Char Int)
addChar frqMap _ "" = frqMap
addChar frqMap idx (c:rest) =
    let frqMap' =
            Map.insertWith (Map.unionWith (+)) idx (Map.singleton c 1) frqMap
     in addChar frqMap' (idx + 1) rest
