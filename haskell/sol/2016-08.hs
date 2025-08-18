{-# LANGUAGE CPP #-}

import Data.Char (isDigit)
import Data.List (intercalate)
import Data.List.Split (condense, dropBlanks, dropDelims, oneOf, split)
import Data.Tuple.Extra (both)
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
    first =
        let accFunc p = (+ (length . filter id) p)
         in foldr accFunc 0 mat
    second = (intercalate "\n" . map characterize) mat
    mat =
        (foldl applyInstruction ((replicate matH . replicate matW) False) .
         lines)
            input

characterize :: [Bool] -> String
characterize [] = ""
characterize row =
    (concatMap boolToChar . take 5) row ++ " " ++ (characterize . drop 5) row

boolToChar :: Bool -> String
boolToChar x =
    if x
        then "@@"
        else "  "

applyInstruction :: [[Bool]] -> String -> [[Bool]]
applyInstruction mat ins =
    let splitter = (split . dropBlanks . condense . dropDelims . oneOf) " =xy"
        insWords = splitter ins
     in case insWords of
            ("rect":rest) -> activate rest mat
            ("rotate":"row":rest) -> rotateRow rest mat
            ("rotate":"column":rest) -> rotateCol rest mat

rotateCol :: [String] -> [[Bool]] -> [[Bool]]
rotateCol [locStr, _, qtyStr] mat =
    let idx = read locStr
        qty = rem (read qtyStr) matH
        col = map (!! idx) mat
        suffix = (reverse . take qty . reverse) col
        prefix = (reverse . drop qty . reverse) col
        accFunc row = (++ drop (idx + 1) row) . (take idx row ++) . (: [])
     in zipWith accFunc mat (suffix ++ prefix)

rotateRow :: [String] -> [[Bool]] -> [[Bool]]
rotateRow [locStr, _, qtyStr] mat =
    let idx = read locStr
        row = mat !! idx
        qty = rem (read qtyStr) matW
        suffix = (reverse . take qty . reverse) row
        prefix = (reverse . drop qty . reverse) row
     in take idx mat ++ [suffix ++ prefix] ++ drop (idx + 1) mat

activate :: [String] -> [[Bool]] -> [[Bool]]
activate [ws, hs] mat =
    let (w, h) = both read (ws, hs)
     in (map ((replicate w True ++) . drop w) . take h) mat ++ drop h mat

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
