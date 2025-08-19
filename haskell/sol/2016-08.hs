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

import Data.List (intercalate)
import Data.List.Split (condense, dropBlanks, dropDelims, oneOf, split)

matH :: Int
matH = 6

matW :: Int
matW = 50

solve :: String -> String
solve input = pairToStr (first, second)
  where
    first =
        let accFunc p = (+ (p |> filter id |> length))
         in foldr accFunc 0 mat
    second = mat |> map characterize |> intercalate "\n"
    mat =
        let mat = False |> replicate matW |> replicate matH
         in input |> lines |> foldl applyInstruction mat

characterize :: [Bool] -> String
characterize [] = ""
characterize row =
    let (first5, rest) = (take 5, drop 5) *$ row
     in concatMap boolToChar first5 ++ " " ++ characterize rest

boolToChar :: Bool -> String
boolToChar True = "@@"
boolToChar False = "  "

applyInstruction :: [[Bool]] -> String -> [[Bool]]
applyInstruction mat ins =
    let splitter = oneOf =:> dropDelims =:> condense =:> dropBlanks =:> split
        insWords = splitter " =xy" ins
     in case insWords of
            ("rect":rest) -> activate rest mat
            ("rotate":"row":rest) -> rotateRow rest mat
            ("rotate":"column":rest) -> rotateCol rest mat

rotateCol :: [String] -> [[Bool]] -> [[Bool]]
rotateCol [locStr, _, qtyStr] mat =
    let idx = read locStr
        qty = rem (read qtyStr) matH
        col = map (!! idx) mat
        (suffix, prefix) =
            col |> reverse |> ((take qty, drop qty) *$) |> (reverse $*)
        accFunc row = (: []) =:> (take idx row ++) =:> (++ drop (idx + 1) row)
     in zipWith accFunc mat (suffix ++ prefix)

rotateRow :: [String] -> [[Bool]] -> [[Bool]]
rotateRow [locStr, _, qtyStr] mat =
    let idx = read locStr
        row = mat !! idx
        qty = rem (read qtyStr) matW
        (suffix, prefix) =
            row |> reverse |> ((take qty, drop qty) *$) |> (reverse $*)
     in take idx mat ++ [suffix ++ prefix] ++ drop (idx + 1) mat

activate :: [String] -> [[Bool]] -> [[Bool]]
activate [ws, hs] mat =
    let (w, h) = read $* (ws, hs)
     in mat |> take h |> map (drop w =:> (replicate w True ++)) |>
        (++ drop h mat)

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
