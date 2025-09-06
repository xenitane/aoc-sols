module Sol where

import Lib

import Crypto.Hash
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

desiredLen :: Int
desiredLen = 8

nc :: Char
nc = '-'

reqPrefix :: String
reqPrefix = "00000"

solve :: String -> String
solve input = pairToStr (first, second)
  where
    first = genPass0 "" 0 input
    second = genPass1 [nc | _ <- [1 .. desiredLen]] 0 input

genPass1 :: String -> Int -> String -> String
genPass1 pass idx prefix
    | nc `notElem` pass = pass
    | otherwise = genPass1 pass' (idx + 1) prefix
  where
    pass' =
        let hash = makeMD5Digest prefix idx
            valid = take 5 hash == reqPrefix
            [b0, b1] = drop 5 hash
            kdx = ord b0 - 48
         in pass
                |> if valid && kdx < desiredLen && pass !! kdx == nc
                       then setAt kdx b1
                       else id

genPass0 :: String -> Int -> String -> String
genPass0 pass idx prefix
    | length pass == desiredLen = pass
    | otherwise = genPass0 pass' (idx + 1) prefix
  where
    pass' =
        let valid = take 5 hash == reqPrefix
            hash = makeMD5Digest prefix idx
         in pass ++ [hash !! 5 | length pass < desiredLen && valid]

makeMD5Digest :: String -> Int -> String
makeMD5Digest prefix idx =
    prefix ++ show idx |> BS.pack |> hashWith MD5 |> show |> take 7
