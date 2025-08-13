{-# LANGUAGE CPP #-}

import Crypto.Hash (Digest, MD5(..), hashWith)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isDigit, ord)
import Data.Map (Map, empty, insertWith, lookup)
import qualified Data.Map as Map
import Lib (pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

solve :: String -> (String, String)
solve input = (first, second)
  where
    (first, _) = foldl (nextByte input) ("", 0) [0 .. 7]
    second = genPass input Map.empty 0

genPass :: String -> Map Int Char -> Int -> String
genPass prefix passMap idx
    | Map.size passMap == 8 = Map.elems passMap
    | otherwise = genPass prefix newMap (idx + 1)
  where
    newMap =
        case makeMD5Digest prefix idx of
            ('0':'0':'0':'0':'0':ii:byte:_) ->
                if isDigit ii && (ord ii < 56)
                    then Map.insertWith (\_ a -> a) (ord ii - 48) byte passMap
                    else passMap
            _ -> passMap

nextByte :: String -> (String, Int) -> Int -> (String, Int)
nextByte prefix (pass, idx) k =
    case makeMD5Digest prefix idx of
        ('0':'0':'0':'0':'0':byte:_) -> (pass ++ [byte], idx + 1)
        _ -> nextByte prefix (pass, idx + 1) k

makeMD5Digest prefix idx =
    (take 7 . show . hashWith MD5 . BS.pack) (prefix ++ show idx)

inputFilePath :: FilePath
inputFilePath = "../inputs/" ++ YEAR ++ "-" ++ DAY ++ ".txt"

testInputFilePath :: FilePath
testInputFilePath = "../test_inputs/" ++ YEAR ++ "-" ++ DAY ++ ".txt"

testOutputFilePath :: FilePath
testOutputFilePath = "../test_outputs/" ++ YEAR ++ "-" ++ DAY ++ ".txt"

runNormalMode :: IO ()
runNormalMode = do
    input <- safeReadFile inputFilePath
    putStr $ pairToStr $ solve input

runTestMode :: IO ()
runTestMode = do
    input <- safeReadFile testInputFilePath
    expected <- trimTrailing <$> safeReadFile testOutputFilePath
    let actual = trimTrailing $ pairToStr $ solve input
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
            exitWith $ ExitFailure 1

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
    exitWith $ ExitFailure 1
#endif
