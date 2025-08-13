{-# LANGUAGE CPP #-}

import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import Lib (pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

type Point = (Int, Int)

solve :: String -> (Int, Int)
solve input = (first, second)
  where
    first = abs xp + abs yp
    second = abs xa + abs ya
    ((xp, yp), Just (xa, ya), _, _) =
        foldl rotateAndMove ((0, 0), Nothing, (0, 1), Set.empty) moves
    moves = map strToMove $ words input

rotateAndMove ::
       (Point, Maybe Point, Point, Set Point)
    -> (Bool, Int)
    -> (Point, Maybe Point, Point, Set Point)
rotateAndMove a (dir, steps) = moveN steps $ rotate dir a

rotate ::
       Bool
    -> (Point, Maybe Point, Point, Set Point)
    -> (Point, Maybe Point, Point, Set Point)
rotate True (pos, act, (dx, dy), mp) = (pos, act, (-dy, dx), mp)
rotate False (pos, act, (dx, dy), mp) = (pos, act, (dy, -dx), mp)

moveN ::
       Int
    -> (Point, Maybe Point, Point, Set Point)
    -> (Point, Maybe Point, Point, Set Point)
moveN 0 res = res
moveN n ((x, y), act, (dx, dy), mp) =
    moveN (n - 1) ((x + dx, y + dy), newAct, (dx, dy), newMp)
  where
    newAct =
        case act of
            Just p -> act
            _ ->
                if Set.member (x + dx, y + dy) mp
                    then Just (x + dx, y + dy)
                    else Nothing
    newMp = Set.insert (x + dx, y + dy) mp

strToMove :: String -> (Bool, Int)
strToMove ('L':rest) = (False, toInt rest)
strToMove ('R':rest) = (True, toInt rest)

toInt :: String -> Int
toInt str = (read . reverse . dropWhile (== ',') . reverse) str :: Int

inputFilePath :: FilePath
inputFilePath = "../inputs/" ++ YEAR ++ "-" ++ DAY ++ ".txt"

testInputFilePath :: FilePath
testInputFilePath = "../test_inputs/" ++ YEAR ++ "-" ++ DAY ++ ".txt"

testOutputFilePath :: FilePath
testOutputFilePath = "../test_outputs/" ++ YEAR ++ "-" ++ DAY ++ ".txt"

runNormalMode :: IO ()
runNormalMode = do
    input <- safeReadFile inputFilePath
    putStr $ pairToStr $ solve input

runTestMode :: IO ()
runTestMode = do
    input <- safeReadFile testInputFilePath
    expected <- trimTrailing <$> safeReadFile testOutputFilePath
    let actual = trimTrailing $ pairToStr $ solve input
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
            exitWith $ ExitFailure 1

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
    exitWith $ ExitFailure 1
#endif
