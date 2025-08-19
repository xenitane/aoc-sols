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

import Data.Char (chr, isDigit, ord)
import Data.List.Split (splitOneOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

targetKeyStr :: String
targetKeyStr = "northpole-object-storage-"

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) = input |> lines |> foldl roomMixup (0, 0)

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
    | otherwise = c |> ord |> (+ (moves - 97)) |> (`rem` 26) |> (+ 97) |> chr

isValidRoom :: String -> Int -> String -> Bool
isValidRoom key sectorId checksum = checksum == expectedChecksum
  where
    expectedChecksum =
        let (_, frqMap) = foldl addCharToFrqMap (Map.empty, Map.empty) key
            joinFunc s = (++ Set.elems s)
         in frqMap |> Map.foldr joinFunc "" |> take (length checksum)

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
        charByCount |>
        (if charFrq == 0
             then id
             else Map.adjust (Set.delete c) charFrq) |>
        Map.insertWith Set.union (charFrq + 1) (Set.singleton c)
    charFrq = charCount |> Map.lookup c |> fromMaybe 0

makeRoom :: String -> (String, Int, String)
makeRoom roomStr =
    let roomValue = roomStr' |> drop (length chars) |> read
        chars = takeWhile (not . isDigit) roomStr'
        [roomStr', checksum, _] = splitOneOf "[]" roomStr
     in (chars, roomValue, checksum)

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
            then pStr' "test passes\n"
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
