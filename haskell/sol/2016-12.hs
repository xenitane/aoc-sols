{-# LANGUAGE CPP #-}

import Data.Char (isLower, ord)
import Data.Tuple.Extra (both)
import Lib (exit, pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

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
        let instructions = (map (makeInstruction . words) . lines) input
         in both
                (head .
                 flip (runInstruction 0) instructions .
                 ([0, 0] ++) . (++ [0]) . (: []))
                (0, 1)

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
                 in (idx + (head . ([1 | uval == 0] ++)) [offset], state)
            ICpy (val, reg) ->
                let uval = unwrapVal state val
                 in (idx + 1, setAt reg uval state)

unwrapVal :: [Int] -> Val -> Int
unwrapVal _ (VRaw v) = v
unwrapVal state (VReg reg) = state !! reg

incAt :: Int -> [Int] -> [Int]
incAt i state = setAt i ((succ . (!! i)) state) state

decAt :: Int -> [Int] -> [Int]
decAt i state = setAt i ((pred . (!! i)) state) state

setAt :: Int -> a -> [a] -> [a]
setAt i v arr = take i arr ++ [v] ++ drop (i + 1) arr

zerrr :: Int
zerrr = ord 'a'

wrapVal val =
    if (isLower . head) val
        then VReg ((ord . head) val - zerrr)
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
