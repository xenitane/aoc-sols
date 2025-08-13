{-# LANGUAGE CPP #-}

import Data.Char (isDigit)
import Lib (exit, pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

matH :: Int
matH = 6

matW :: Int
matW = 50

solve :: String -> String
solve input = pairToStr (first, second)
  where
    first = (foldl (\p x -> p + (length . filter id) x) 0) mat
    second = (drop 1 . foldl (\p x -> p ++ "\n" ++ x) "" . map characterize) mat
    mat =
        (foldl applyInstruction ((replicate matH . replicate matW) False) .
         lines)
            input

characterize :: [Bool] -> String
characterize [] = ""
characterize row =
    (concat . map boolToChar . take 5) row ++ " " ++ (characterize . drop 5) row

boolToChar :: Bool -> String
boolToChar x =
    if x
        then "@@"
        else "  "

applyInstruction :: [[Bool]] -> String -> [[Bool]]
applyInstruction mat ins =
    case takeWhile (/= ' ') ins of
        "rect" -> activate rest mat
        "rotate" ->
            case takeWhile (/= ' ') rest of
                "row" -> rotateRow nrest mat
                "column" -> rotateCol nrest mat
  where
    nrest = (drop 1 . dropWhile (/= '=')) rest
    rest = (drop 1 . dropWhile (/= ' ')) ins

rotateCol :: String -> [[Bool]] -> [[Bool]]
rotateCol locQty mat =
    (map (\(row, val) -> take idx row ++ [val] ++ drop (idx + 1) row) . zip mat)
        (suffix ++ prefix)
  where
    prefix = (reverse . drop qty . reverse) col
    suffix = (reverse . take qty . reverse) col
    col = map (!! idx) mat
    qty = ((read . drop 4 . dropWhile isDigit) locQty :: Int) `rem` matH
    idx = (read . takeWhile isDigit) locQty :: Int

rotateRow :: String -> [[Bool]] -> [[Bool]]
rotateRow locQty mat = take idx mat ++ [suffix ++ prefix] ++ drop (idx + 1) mat
  where
    prefix = (reverse . drop qty . reverse) row
    suffix = (reverse . take qty . reverse) row
    row = mat !! idx
    qty = ((read . drop 4 . dropWhile isDigit) locQty :: Int) `rem` matW
    idx = (read . takeWhile isDigit) locQty :: Int

activate :: String -> [[Bool]] -> [[Bool]]
activate dim mat =
    ((map (\row -> replicate x True ++ drop x row) . take y) mat) ++ drop y mat
  where
    y = (read . drop 1 . dropWhile isDigit) dim :: Int
    x = (read . takeWhile isDigit) dim :: Int

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
