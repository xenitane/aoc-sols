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

import Data.List (elemIndex)

solve :: String -> String
solve input = (first, second) |> pairToStr
  where
    first = "abcdefgh" |> runInstruction instructions
    second = "fbgdceah" |> revertInstruction (reverse instructions)
    instructions = input |> lines |> map (words =:> makeInstruction)

revertInstruction :: [Instruction] -> String -> String
revertInstruction [] pass = pass
revertInstruction (x:xs) pass = revertInstruction xs pass'
  where
    pass' =
        case x of
            SwpP (x, y) ->
                let (a, b) = (pass !!) $* (x, y)
                 in pass |> setAt x b |> setAt y a
            SwpL (a, b) ->
                let (Just x, Just y) = flip elemIndex pass $* (a, b)
                 in pass |> setAt x b |> setAt y a
            RotN (dir, n) ->
                let n' = mod n (length pass)
                 in if dir
                        then let pass' = reverse pass
                              in ((drop n' pass') ++ (take n' pass')) |> reverse
                        else (drop n' pass) ++ (take n' pass)
            RotC c ->
                let Just x = elemIndex c pass
                    shift =
                        case x of
                            0 -> 1
                            x'
                                | even x' -> 5 + div x 2
                                | otherwise -> 1 + div x 2
                 in (drop shift pass) ++ (take shift pass)
            Rev (x, y) ->
                let prefix = take x pass
                    suffix = drop (y + 1) pass
                    seg = pass |> take (y + 1) |> drop x |> reverse
                 in prefix ++ seg ++ suffix
            Mov (y, x) ->
                let pass' = (take x pass) ++ (drop (x + 1) pass)
                    char = pass !! x
                    prefix = take y pass'
                    suffix = drop y pass'
                 in prefix ++ [char] ++ suffix

runInstruction :: [Instruction] -> String -> String
runInstruction [] pass = pass
runInstruction (x:xs) pass = runInstruction xs pass'
  where
    pass' =
        case x of
            SwpP (x, y) ->
                let (a, b) = (pass !!) $* (x, y)
                 in pass |> setAt x b |> setAt y a
            SwpL (a, b) ->
                let (Just x, Just y) = flip elemIndex pass $* (a, b)
                 in pass |> setAt x b |> setAt y a
            RotN (dir, n) ->
                let n' = mod n (length pass)
                 in if dir
                        then (drop n' pass) ++ (take n' pass)
                        else let pass' = reverse pass
                              in ((drop n' pass') ++ (take n' pass')) |> reverse
            RotC c ->
                let Just x = elemIndex c pass
                    x' = mod (x + 1 + head ([1 | x > 3] ++ [0])) (length pass)
                    pass' = reverse pass
                 in ((drop x' pass') ++ (take x' pass')) |> reverse
            Rev (x, y) ->
                let prefix = take x pass
                    suffix = drop (y + 1) pass
                    seg = pass |> take (y + 1) |> drop x |> reverse
                 in prefix ++ seg ++ suffix
            Mov (x, y) ->
                let pass' = (take x pass) ++ (drop (x + 1) pass)
                    char = pass !! x
                    prefix = take y pass'
                    suffix = drop y pass'
                 in prefix ++ [char] ++ suffix

data Instruction
    = SwpP (Int, Int)
    | SwpL (Pair Char)
    | RotN (Bool, Int)
    | RotC Char
    | Rev (Pair Int)
    | Mov (Pair Int)

makeInstruction :: [String] -> Instruction
makeInstruction ["swap", "position", a, _, _, b] =
    (a, b) |> ((read @Int) $*) |> SwpP
makeInstruction ["swap", "letter", [a], _, _, [b]] = SwpL (a, b)
makeInstruction ["rotate", dir, steps, _] = RotN (dir == "left", read steps)
makeInstruction ["rotate", "based", _, _, _, _, [a]] = RotC a
makeInstruction ["reverse", _, a, _, b] = (a, b) |> ((read @Int) $*) |> Rev
makeInstruction ["move", _, a, _, _, b] = (a, b) |> ((read @Int) $*) |> Mov

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
            then pStr' "test passed\n"
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
