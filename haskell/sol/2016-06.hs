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

import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) =
        input |> lines |> foldl mergeLines Map.empty |> Map.elems |>
        foldl getMaxMin ("", "")

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
