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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

keysReq :: Int
keysReq = 64

iterQtys :: (Int, Int)
iterQtys = (1, 2017)

candidadtuereLimit :: Int
candidadtuereLimit = 1000

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) = findIndexOfLastKey 0 0 Map.empty input $* iterQtys

findIndexOfLastKey ::
       Int -> Int -> Map Int (Maybe (Char, Set Char)) -> String -> Int -> Int
findIndexOfLastKey idx keys indexTripPentCache prefix iter
    | keys == keysReq = idx - 1
    | otherwise = findIndexOfLastKey idx' keys' indexTripPentCache' prefix iter
  where
    (idx', keys', indexTripPentCache') =
        let val = computeTripPentData iter (prefix ++ show idx)
            (tripPent, tempIndexTripPentCache) =
                case Map.lookup idx indexTripPentCache of
                    Just v -> (v, indexTripPentCache)
                    _ -> (val, Map.insert idx val indexTripPentCache)
         in case tripPent of
                (Just (tripChar, _)) ->
                    let (keys', indexTripPentCache') =
                            verifyKey
                                tripChar
                                0
                                idx
                                keys
                                tempIndexTripPentCache
                                prefix
                                iter
                     in (idx + 1, keys', indexTripPentCache')
                _ -> (idx + 1, keys, tempIndexTripPentCache)

verifyKey ::
       Char
    -> Int
    -> Int
    -> Int
    -> Map Int (Maybe (Char, Set Char))
    -> String
    -> Int
    -> (Int, Map Int (Maybe (Char, Set Char)))
verifyKey tripChar sessions idx keys indexTripPentCache prefix iter
    | sessions == candidadtuereLimit = (keys, indexTripPentCache)
    | otherwise =
        let keyId = idx + sessions + 1
            val = computeTripPentData iter (prefix ++ show keyId)
            (tripPent, tempIndexTripPentCache) =
                case Map.lookup keyId indexTripPentCache of
                    Just v -> (v, indexTripPentCache)
                    _ -> (val, Map.insert keyId val indexTripPentCache)
         in case tripPent of
                Just (_, pentChars)
                    | Set.member tripChar pentChars ->
                        (keys + 1, tempIndexTripPentCache)
                _ ->
                    verifyKey
                        tripChar
                        (sessions + 1)
                        idx
                        keys
                        tempIndexTripPentCache
                        prefix
                        iter

computeTripPentData :: Int -> String -> Maybe (Char, Set Char)
computeTripPentData iter str =
    let str' = makeMD5DigestNtimes iter str
     in (, pentChars str') <$> mTripChar str'

pentChars :: String -> Set Char
pentChars str
    | length str == 4 = Set.empty
    | otherwise =
        str |> drop 1 |> pentChars |>
        (if str |> take 5 |> all (== head str)
             then Set.insert (head str)
             else id)

mTripChar :: String -> Maybe Char
mTripChar str
    | length str == 2 = Nothing
    | otherwise =
        str |>
        if str |> take 3 |> all (== head str)
            then head =:> Just
            else drop 1 =:> mTripChar

makeMD5DigestNtimes :: Int -> String -> String
makeMD5DigestNtimes n str
    | n == 0 = str
    | otherwise =
        str |> BS.pack |> hashWith MD5 |> show |> makeMD5DigestNtimes (n - 1)

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
