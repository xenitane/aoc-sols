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

solve :: String -> String
solve input = (first, second) |> pairToStr
  where
    -- look at the puzzle input for patterns and you'll find multiplication
    -- so modify the input for that
    (first, second) =
        let instructions = input |> lines |> map (words =:> makeInstruction)
            f x = runInstruction 0 [x, 0, 0, 0] instructions |> head
         in f $* (7, 12)

data Val
    = VReg Int
    | VRaw Int

data Instruction
    = ICpy (Pair Val)
    | IInc Val
    | IDec Val
    | IJnz (Pair Val)
    | ITgl Val
    | IMul (Val, Val, Val)

runInstruction :: Int -> [Int] -> [Instruction] -> [Int]
runInstruction idx state instructions
    | idx < 0 || idx >= length instructions = state
    | otherwise = runInstruction idx' state' instructions'
  where
    (idx', state', instructions') =
        case instructions !! idx of
            IInc (VReg r) -> (idx + 1, incAt r state, instructions)
            IDec (VReg r) -> (idx + 1, decAt r state, instructions)
            ICpy (v, VReg r) ->
                let val = unwrapVal state v
                 in (idx + 1, setAt r val state, instructions)
            IJnz vo ->
                let (val, offset) = unwrapVal state $* vo
                 in ( idx + ([offset] |> ([1 | val == 0] ++) |> head)
                    , state
                    , instructions)
            ITgl o ->
                let offset = unwrapVal state o
                 in (idx + 1, state, toggleIns (idx + offset) instructions)
            IMul (a, b, VReg r) ->
                let (a', b') = unwrapVal state $* (a, b)
                 in (idx + 1, setAt r (a' * b') state, instructions)
            _ -> (idx + 1, state, instructions)

toggleIns :: Int -> [Instruction] -> [Instruction]
toggleIns idx instructions
    | idx < 0 || idx >= length instructions = instructions
    | otherwise = setAt idx instruction' instructions
  where
    instruction' =
        case instructions !! idx of
            IInc t -> IDec t
            IDec t -> IInc t
            ICpy t -> IJnz t
            IJnz t -> ICpy t
            ITgl t -> IInc t
            IMul t -> IMul t

incAt :: Int -> [Int] -> [Int]
incAt i state = setAt i ((state !! i) |> succ) state

decAt :: Int -> [Int] -> [Int]
decAt i state = setAt i ((state !! i) |> pred) state

unwrapVal :: [Int] -> Val -> Int
unwrapVal _ (VRaw v) = v
unwrapVal state (VReg i) = state !! i

wrapVal :: String -> Val
wrapVal val =
    if val |> head |> isLower
        then VReg (val |> head |> ord |> flip (-) zerrr)
        else VRaw (read val)
  where
    zerrr = ord 'a'

makeInstruction :: [String] -> Instruction
makeInstruction ["cpy", val, target] = (val, target) |> (wrapVal $*) |> ICpy
makeInstruction ["inc", target] = target |> wrapVal |> IInc
makeInstruction ["dec", target] = target |> wrapVal |> IDec
makeInstruction ["jnz", val, offset] = (val, offset) |> (wrapVal $*) |> IJnz
makeInstruction ["tgl", offset] = offset |> wrapVal |> ITgl
makeInstruction ["mul", a, b, target] =
    IMul (wrapVal a, wrapVal b, wrapVal target)

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
