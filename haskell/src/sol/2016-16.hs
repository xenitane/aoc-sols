module Sol where

import Lib

import Data.Char

diskLength :: Pair Int
diskLength = (272, 35651584)

solve :: String -> String
solve input = (first, second) |> pairToStr
  where
    (first, second) = generateChecksum input $* diskLength

generateChecksum :: String -> Int -> String
generateChecksum str minLen
    | length str >= minLen = str |> take minLen |> makeChecksum
    | otherwise =
        let str' =
                str
                    |> reverse
                    |> map (ord =:> (97 -) =:> chr)
                    |> ("0" ++)
                    |> (str ++)
         in generateChecksum str' minLen

makeChecksum :: String -> String
makeChecksum str
    | str |> length |> even = str |> shorten |> makeChecksum
    | otherwise = str

shorten :: String -> String
shorten [] = []
shorten (b0:b1:rest) =
    (if b0 == b1
         then "1"
         else "0")
        ++ shorten rest
