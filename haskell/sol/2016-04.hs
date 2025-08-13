{-# LANGUAGE CPP #-}

import Data.Char (chr, isDigit, ord)
import Data.Map (Map, elems, empty, insert, lookup)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Maybe (fromMaybe)
import Data.Set (Set, delete, empty, insert, toAscList)
import qualified Data.Set as Set
import Lib (pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

solve :: String -> (Int, Int)
solve input = (first, second)
  where
    first =
        (foldl (\prev (_, sectorId, _) -> prev + sectorId) 0 .
         filter isValidRoom)
            rooms
    (_, second) =
        (last .
         filter (\(key, _) -> key == "northpole-object-storage-") .
         map (\(key, sectorId, _) -> (shiftCipher sectorId key, sectorId)))
            rooms
    rooms = map makeRoom $ lines input

shiftCipher :: Int -> String -> String
shiftCipher moves str
    | moves >= 26 = shiftCipher (moves `rem` 26) str
    | otherwise = map (shiftChar moves) str

shiftChar :: Int -> Char -> Char
shiftChar moves '-'
    | (moves `rem` 2) == 1 = ' '
    | otherwise = '-'
shiftChar moves c = chr $ 97 + ((moves + ord c - 97) `rem` 26)

isValidRoom :: (String, Int, String) -> Bool
isValidRoom (chars, _, checksum) = checksum == expectedChecksum
  where
    expectedChecksum =
        (take (length checksum) .
         foldl (\str set -> str ++ Set.toAscList set) "" . reverse . Map.elems)
            frqMap
    (_, frqMap) = foldl addCharToFrqMap (Map.empty, Map.empty) chars

addCharToFrqMap ::
       (Map Char Int, Map Int (Set Char))
    -> Char
    -> (Map Char Int, Map Int (Set Char))
addCharToFrqMap (ma, mb) c
    | c == '-' = (ma, mb)
    | otherwise = (nma, nmb)
  where
    nma = Map.insert c (oldFrq + 1) ma
    nmb = Map.insert oldFrq newSetOldF $ Map.insert (oldFrq + 1) newSetNewF mb
    newSetNewF =
        Set.insert c $
        case Map.lookup (oldFrq + 1) mb of
            Just s -> s
            Nothing -> Set.empty
    newSetOldF =
        Set.delete c $
        case Map.lookup oldFrq mb of
            Just s -> s
            Nothing -> Set.empty
    oldFrq = Maybe.fromMaybe 0 $ Map.lookup c ma

makeRoom :: String -> (String, Int, String)
makeRoom roomStr = (chars, roomValue, checksum)
  where
    checksum = getCheckSum "" rest1
    (roomValue, rest1) = getInt 0 rest0
    (chars, rest0) = extractChars "" roomStr

extractChars :: String -> String -> (String, String)
extractChars s "" = (s, "")
extractChars s (h:r)
    | isDigit h = (s, h : r)
    | otherwise = extractChars (s ++ [h]) r

getInt :: Int -> String -> (Int, String)
getInt n "" = (n, "")
getInt n ('[':r) = (n, r)
getInt n (h:r) = getInt ((n * 10) + (ord h - 48)) r

getCheckSum :: String -> String -> String
getCheckSum str "" = str
getCheckSum str "]" = str
getCheckSum str (h:r) = getCheckSum r $ str ++ [h]

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
