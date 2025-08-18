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

targetKeyStr :: String
targetKeyStr = "northpole-object-storage-"

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) = (foldl roomMixup (0, 0) . lines) input

roomMixup :: (Int, Int) -> String -> (Int, Int)
roomMixup (validRoomSum, targetRoomSector) roomStr =
    (validRoomSum + validatedRoomId, targetRoomSector + targetedRoomId)
  where
    targetedRoomId =
        if shiftCipher sectorId key == targetKeyStr
            then sectorId
            else 0
    validatedRoomId =
        if isValidRoom key sectorId checksum
            then sectorId
            else 0
    (key, sectorId, checksum) = makeRoom roomStr

shiftCipher :: Int -> String -> String
shiftCipher moves str
    | moves >= 26 = shiftCipher (rem moves 26) str
    | otherwise = map (shiftChar moves) str

shiftChar :: Int -> Char -> Char
shiftChar moves c
    | c == '-' = "- " !! rem moves 2
    | otherwise = (chr . (+ 97) . (`rem` 26) . (+ (moves - 97)) . ord) c

isValidRoom :: String -> Int -> String -> Bool
isValidRoom key sectorId checksum = checksum == expectedChecksum
  where
    expectedChecksum =
        let (_, frqMap) = foldl addCharToFrqMap (Map.empty, Map.empty) key
            joinFunc s = (++ Set.elems s)
         in (take (length checksum) . Map.foldr joinFunc "") frqMap

addCharToFrqMap ::
       (Map Char Int, Map Int (Set Char))
    -> Char
    -> (Map Char Int, Map Int (Set Char))
addCharToFrqMap (charCount, charByCount) c
    | c == '-' = (charCount, charByCount)
    | otherwise = (charCount', charByCount')
  where
    charCount' = Map.insert c (charFrq + 1) charCount
    charByCount' =
        (Map.insertWith Set.union (charFrq + 1) (Set.singleton c) .
         (if charFrq == 0
              then id
              else Map.adjust (Set.delete c) charFrq))
            charByCount
    charFrq = (Maybe.fromMaybe 0 . Map.lookup c) charCount

makeRoom :: String -> (String, Int, String)
makeRoom roomStr =
    let roomValue =
            (read . takeWhile isDigit . dropWhile (not . isDigit)) roomStr'
        chars = takeWhile (not . isDigit) roomStr'
        [roomStr', checksum, _] = splitOneOf "[]" roomStr
     in (chars, roomValue, checksum)

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
