{-# LANGUAGE CPP #-}

import Data.Char (chr, isDigit, ord)
import Data.List.Split (splitOneOf)
import Data.Map (Map, elems, empty, insert, lookup)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Maybe (fromMaybe)
import Data.Set (Set, delete, empty, insert, toAscList)
import qualified Data.Set as Set
import Lib (exit, pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) = (foldl roomMixup (0, 0) . lines) input

roomMixup :: (Int, Int) -> String -> (Int, Int)
roomMixup (validRoomSum, targetRoomSector) roomStr =
    (validRoomSum + validatedRoomId, targetRoomSector + targetedRoomId)
  where
    targetedRoomId =
        if shiftCipher sectorId key == "northpole-object-storage-"
            then sectorId
            else 0
    validatedRoomId =
        if isValidRoom key sectorId checksum
            then sectorId
            else 0
    (key, sectorId, checksum) = makeRoom roomStr

shiftCipher :: Int -> String -> String
shiftCipher moves str
    | moves >= 26 = shiftCipher (moves `rem` 26) str
    | otherwise = map (shiftChar moves) str

shiftChar :: Int -> Char -> Char
shiftChar moves '-'
    | (moves `rem` 2) == 1 = ' '
    | otherwise = '-'
shiftChar moves c = (chr . (\x -> 97 + (ord x + moves - 97) `rem` 26)) c

isValidRoom :: String -> Int -> String -> Bool
isValidRoom key sectorId checksum = checksum == expectedChecksum
  where
    expectedChecksum =
        (take (length checksum) .
         foldl (\str set -> str ++ Set.toAscList set) "" . reverse . Map.elems)
            frqMap
    (_, frqMap) = foldl addCharToFrqMap (Map.empty, Map.empty) key

addCharToFrqMap ::
       (Map Char Int, Map Int (Set Char))
    -> Char
    -> (Map Char Int, Map Int (Set Char))
addCharToFrqMap (ma, mb) c
    | c == '-' = (ma, mb)
    | otherwise = (nma, nmb)
  where
    nma = Map.insert c (oldFrq + 1) ma
    nmb = (Map.insert oldFrq newSetOldF . Map.insert (oldFrq + 1) newSetNewF) mb
    newSetNewF =
        (Set.insert c . Maybe.fromMaybe Set.empty . Map.lookup (oldFrq + 1)) mb
    newSetOldF =
        (Set.delete c . Maybe.fromMaybe Set.empty . Map.lookup oldFrq) mb
    oldFrq = (Maybe.fromMaybe 0 . Map.lookup c) ma

makeRoom :: String -> (String, Int, String)
makeRoom roomStr = (chars, roomValue, checksum)
  where
    roomValue = (read . takeWhile isDigit . dropWhile (not . isDigit)) rest
    chars = takeWhile (not . isDigit) rest
    [rest, checksum, _] = splitOneOf "[]" roomStr

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
