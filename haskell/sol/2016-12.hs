{-# LANGUAGE CPP #-}

import Data.Char (isLower, ord)
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
    first = (head . runInstruction 0 [0, 0, 0, 0]) instructions
    second = (head . runInstruction 0 [0, 0, 1, 0]) instructions
    instructions = (map (makeInstruction . words) . lines) input

runInstruction :: Int -> [Int] -> [Instruction] -> [Int]
runInstruction idx state instructions
    | idx >= length instructions = state
    | otherwise = runInstruction newIdx newState instructions
  where
    (newIdx, newState) =
        case instructions !! idx of
            IInc reg -> (idx + 1, incAt reg state)
            IDec reg -> (idx + 1, decAt reg state)
            IJnz (val, offset) ->
                ( idx +
                  if unwrapVal state val /= 0
                      then offset
                      else 1
                , state)
            ICpy (val, reg) -> (idx + 1, setAt reg (unwrapVal state val) state)

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

makeInstruction :: [String] -> Instruction
makeInstruction ["cpy", val, [target]] = ICpy (vVal, ord target - zerrr)
  where
    vVal =
        if (isLower . head) val
            then VReg ((ord . head) val - zerrr)
            else VRaw (read val)
makeInstruction ["inc", [target]] = IInc (ord target - zerrr)
makeInstruction ["dec", [target]] = IDec (ord target - zerrr)
makeInstruction ["jnz", val, offset] = IJnz (vVal, read offset)
  where
    vVal =
        if (isLower . head) val
            then VReg ((ord . head) val - zerrr)
            else VRaw (read val)

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
