{-# LANGUAGE CPP #-}

import Crypto.Hash (Digest, MD5(..), hashWith)
import Data.ByteArray (convert)
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (both)
import Lib (exit, pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

keysReq :: Int
keysReq = 64

iterQtys :: (Int, Int)
iterQtys = (1, 2017)

candidadtuereLimit :: Int
candidadtuereLimit = 1000

solve :: String -> String
solve input = pairToStr (first, second)
  where
    -- second = 0 :: Int
    (first, second) = both (findIndexOfLastKey 0 0 Map.empty input) iterQtys

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
     in (fmap (, pentChars str') . mTripChar) str'

pentChars :: String -> Set Char
pentChars str
    | length str == 4 = Set.empty
    | otherwise =
        (if (all (== head str) . take 5) str
             then Set.insert (head str)
             else id)
            ((pentChars . drop 1) str)

mTripChar :: String -> Maybe Char
mTripChar str
    | length str == 2 = Nothing
    | otherwise =
        (if (all (== head str) . take 3) str
             then Just . head
             else mTripChar . drop 1)
            str

makeMD5DigestNtimes :: Int -> String -> String
makeMD5DigestNtimes n str
    | n == 0 = str
    | otherwise =
        (makeMD5DigestNtimes (n - 1) . show . hashWith MD5 . BS.pack) str

inputFilePath :: FilePath
inputFilePath = "../inputs/" ++ YEAR ++ "-" ++ DAY ++ ".txt"

testInputFilePath :: FilePath
testInputFilePath = "../test_inputs/" ++ YEAR ++ "-" ++ DAY ++ ".txt"

testOutputFilePath :: FilePath
testOutputFilePath = "../test_outputs/" ++ YEAR ++ "-" ++ DAY ++ ".txt"

runNormalMode :: IO ()
runNormalMode = do
    input <- safeReadFile inputFilePath
    (putStr . solve) input

runTestMode :: IO ()
runTestMode = do
    input <- safeReadFile testInputFilePath
    expectedIO <- safeReadFile testOutputFilePath
    let expected = trimTrailing expectedIO
    let actual = (trimTrailing . solve) input
    if actual == expected
        then putStrLn "test passed"
        else do
            putStrLn "test failed"
            putStrLn "Expected:"
            putStrLn "--------------"
            putStrLn expected
            putStrLn "--------------"
            putStrLn "Got:"
            putStrLn "--------------"
            putStrLn actual
            putStrLn "--------------"
            exit 1

main :: IO ()
#if defined YEAR && defined DAY
main = do
    testMode <- lookupEnv "TEST_MODE"
    case testMode of
        Just "1" -> runTestMode
        _ -> runNormalMode
#else
main = do
    putStrLn "essential variables not defined"
    exit 1
#endif
