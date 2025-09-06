module Sol where

import Lib

import Data.Char

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) = decompress input

decompress :: String -> (Int, Int)
decompress "" = (0, 0)
decompress ('(':rest) =
    let rawRest = rest |> dropWhile (/= ')') |> drop 1
     in rawRest
            |> drop len
            |> decompress
            |> boths
                   (+)
                   ((* times) $* (len, rawRest |> take len |> decompress |> snd))
  where
    times = rest |> dropWhile isDigit |> drop 1 |> takeWhile isDigit |> read
    len = rest |> takeWhile isDigit |> read
decompress (h:rest) = rest |> decompress |> (succ $*)
