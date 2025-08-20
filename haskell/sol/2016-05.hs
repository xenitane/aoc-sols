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

import Crypto.Hash (MD5(..), hashWith)
import qualified Data.ByteString.Char8 as BS
import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as Map

desiredLen :: Int
desiredLen = 8

nc :: Char
nc = '-'

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
            valid = take 5 hash == "00000"
            [b0, b1] = drop 5 hash
            kdx = ord b0 - 48
         in pass |>
            if valid && kdx < desiredLen && pass !! kdx == nc
                then setAt kdx b1
                else id

genPass0 :: String -> Int -> String -> String
genPass0 pass idx prefix
    | length pass == desiredLen = pass
    | otherwise = genPass0 pass' (idx + 1) prefix
  where
    pass' =
        let valid = take 5 hash == "00000"
            hash = makeMD5Digest prefix idx
         in pass ++ [hash !! 5 | length pass < desiredLen && valid]

makeMD5Digest :: String -> Int -> String
makeMD5Digest prefix idx =
    prefix ++ show idx |> BS.pack |> hashWith MD5 |> show |> take 7

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
