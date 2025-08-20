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

import Data.Char (isLower, ord)

data Val
    = VReg Int
    | VRaw Int

data Instruction
    = ICpy (Val, Int)
    | IInc Int
    | IDec Int
    | IJnz (Val, Int)

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) =
        let instructions = input |> lines |> map (words =:> makeInstruction)
            f x = runInstruction 0 [0, 0, x, 0] instructions |> head
         in f $* (0, 1)

runInstruction :: Int -> [Int] -> [Instruction] -> [Int]
runInstruction idx state instructions
    | idx >= length instructions = state
    | otherwise = runInstruction idx' state' instructions
  where
    (idx', state') =
        case instructions !! idx of
            IInc reg -> (idx + 1, incAt reg state)
            IDec reg -> (idx + 1, decAt reg state)
            IJnz (val, offset) ->
                let uval = unwrapVal state val
                 in (idx + ([offset] |> ([1 | uval == 0] ++) |> head), state)
            ICpy (val, reg) ->
                let uval = unwrapVal state val
                 in (idx + 1, setAt reg uval state)

unwrapVal :: [Int] -> Val -> Int
unwrapVal _ (VRaw v) = v
unwrapVal state (VReg reg) = state !! reg

incAt :: Int -> [Int] -> [Int]
incAt i state = setAt i ((state !! i) |> succ) state

decAt :: Int -> [Int] -> [Int]
decAt i state = setAt i ((state !! i) |> pred) state

zerrr :: Int
zerrr = ord 'a'

wrapVal val =
    if (isLower . head) val
        then VReg (val |> head |> ord |> (zerrr -))
        else VRaw (read val)

makeInstruction :: [String] -> Instruction
makeInstruction ["cpy", val, [target]] =
    let vVal = wrapVal val
     in ICpy (vVal, ord target - zerrr)
makeInstruction ["inc", [target]] = IInc (ord target - zerrr)
makeInstruction ["dec", [target]] = IDec (ord target - zerrr)
makeInstruction ["jnz", val, offset] =
    let vVal = wrapVal val
     in IJnz (vVal, read offset)

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
