{-# LANGUAGE CPP #-}

import Data.Char (isDigit)
import Data.List.Split
    ( condense
    , dropDelims
    , dropFinalBlank
    , dropInitBlank
    , oneOf
    , split
    )
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
    first = foldl (\p x -> p + (length . filter id) x) 0 mat
    second = (drop 1 . foldl (\p x -> p ++ "\n" ++ x) "" . map characterize) mat
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
    case insKind of
        "rect" -> activate rest mat
        "rotate" ->
            case head rest of
                "row" -> rotateRow (drop 1 rest) mat
                "column" -> rotateCol (drop 1 rest) mat
  where
    (insKind:rest) =
        (split . dropInitBlank . dropFinalBlank . condense . dropDelims . oneOf)
            " =xy"
            ins

rotateCol :: [String] -> [[Bool]] -> [[Bool]]
rotateCol [locStr, _, qtyStr] mat =
    zipWith
        (\row val -> take idx row ++ [val] ++ drop (idx + 1) row)
        mat
        (suffix ++ prefix)
  where
    prefix = (reverse . drop qty . reverse) col
    suffix = (reverse . take qty . reverse) col
    col = map (!! idx) mat
    qty = rem (read qtyStr) matH
    idx = read locStr

rotateRow :: [String] -> [[Bool]] -> [[Bool]]
rotateRow [locStr, _, qtyStr] mat =
    take idx mat ++ [suffix ++ prefix] ++ drop (idx + 1) mat
  where
    prefix = (reverse . drop qty . reverse) row
    suffix = (reverse . take qty . reverse) row
    row = mat !! idx
    qty = rem (read qtyStr) matW
    idx = read locStr

activate :: [String] -> [[Bool]] -> [[Bool]]
activate [xs, ys] mat =
    (map (\row -> replicate x True ++ drop x row) . take y) mat ++ drop y mat
  where
    y = read ys
    x = read xs

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
