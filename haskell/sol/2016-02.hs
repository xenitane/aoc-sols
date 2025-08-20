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

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

moveId :: Char -> Int
moveId 'L' = 0
moveId 'U' = 1
moveId 'R' = 2
moveId 'D' = 3

moveMapsG =
    Map.fromList $*
    ( [ ('1', "1124")
      , ('2', "1235")
      , ('3', "2336")
      , ('4', "4157")
      , ('5', "4268")
      , ('6', "5369")
      , ('7', "7487")
      , ('8', "7598")
      , ('9', "8699")
      ]
    , [ ('1', "1113")
      , ('2', "2236")
      , ('3', "2147")
      , ('4', "3448")
      , ('5', "5565")
      , ('6', "527A")
      , ('7', "638B")
      , ('8', "749C")
      , ('9', "8999")
      , ('A', "A6BA")
      , ('B', "A7CD")
      , ('C', "B8CC")
      , ('D', "DBDD")
      ])

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) = input |> lines |> foldl (reposition moveMapsG) ("", "")

reposition :: Pair (Map Char String) -> Pair String -> String -> Pair String
reposition moveMaps ("", "") str =
    str |> moveFromAccToCode moveMaps ('5', '5') |> ((: []) $*)
reposition moveMaps state str =
    let append a = (: []) =:> (a ++)
     in str |> moveFromAccToCode moveMaps (last $* state) |> boths append state

moveFromAccToCode :: Pair (Map Char String) -> Pair Char -> String -> Pair Char
moveFromAccToCode _ c "" = c
moveFromAccToCode moveMaps c (h:r) =
    let c' = c |> next moveMaps (moveId h)
     in r |> moveFromAccToCode moveMaps c'

next :: Pair (Map Char String) -> Int -> Pair Char -> Pair Char
next mms idx keys =
    let ncs x = x |> fromMaybe "0000" |> (!! idx)
     in mms |> boths Map.lookup keys |> (ncs $*)

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
