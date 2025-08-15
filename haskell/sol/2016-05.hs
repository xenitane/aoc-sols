{-# LANGUAGE CPP #-}

import Crypto.Hash (Digest, MD5(..), hashWith)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isDigit, ord)
import Data.Map (Map, empty, insertWith, size)
import qualified Data.Map as Map
import Lib (exit, pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

type Pair a = (a, a)

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
    | otherwise = genPass1 newPass (idx + 1) prefix
  where
    newPass =
        (if valid && kdx < desiredLen && pass !! kdx == nc
             then setAt kdx b1
             else id)
            pass
    valid = take 5 hash == "00000"
    kdx = ord b0 - 48
    [b0, b1] = drop 5 hash
    hash = makeMD5Digest prefix idx

genPass0 :: String -> Int -> String -> String
genPass0 pass idx prefix
    | length pass == desiredLen = pass
    | otherwise = genPass0 newPass (idx + 1) prefix
  where
    newPass = pass ++ [b | length pass < desiredLen && valid]
    [b, _] = drop 5 hash
    valid = take 5 hash == "00000"
    hash = makeMD5Digest prefix idx

makeMD5Digest :: String -> Int -> String
makeMD5Digest prefix idx =
    (take 7 . show . hashWith MD5 . BS.pack) (prefix ++ show idx)

setAt :: Int -> a -> [a] -> [a]
setAt idx val arr = take idx arr ++ [val] ++ drop (idx + 1) arr

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
