module Sol where

import Lib

import Data.Char

data Val
    = VReg Int
    | VRaw Int

data Instruction
    = ICpy (Pair Val)
    | IInc Val
    | IDec Val
    | IJnz (Pair Val)

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
            IInc (VReg reg) -> (idx + 1, incAt reg state)
            IDec (VReg reg) -> (idx + 1, decAt reg state)
            IJnz vo ->
                let (val, offset) = unwrapVal state $* vo
                 in (idx + ([offset] |> ([1 | val == 0] ++) |> head), state)
            ICpy (v, VReg reg) ->
                let val = unwrapVal state v
                 in (idx + 1, setAt reg val state)
            _ -> (idx + 1, state)

incAt :: Int -> [Int] -> [Int]
incAt i state = setAt i ((state !! i) |> succ) state

decAt :: Int -> [Int] -> [Int]
decAt i state = setAt i ((state !! i) |> pred) state

unwrapVal :: [Int] -> Val -> Int
unwrapVal _ (VRaw v) = v
unwrapVal state (VReg reg) = state !! reg

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
