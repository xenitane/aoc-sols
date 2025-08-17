{-# LANGUAGE CPP #-}

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import Lib (exit, pairToStr, safeReadFile, trimTrailing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)

type Pair a = (a, a)

type Point = Pair Int

solve :: String -> String
solve input = pairToStr (first, second)
  where
    first = abs x + abs y
    second = abs x' + abs y'
    ((x, y), Just (x', y'), _, _) =
        (foldl rotateAndMove ((0, 0), Nothing, (0, 1), Set.empty) . splitOn ", ")
            input

rotateAndMove ::
       (Point, Maybe Point, Point, Set Point)
    -> String
    -> (Point, Maybe Point, Point, Set Point)
rotateAndMove res (dir:steps) = (moveN (read steps) . rotate (dir == 'L')) res

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
    moveN (n - 1) ((x', y'), act', (dx, dy), mp')
  where
    act' =
        case act of
            Just p -> act
            _ ->
                if Set.member (x', y') mp
                    then Just (x', y')
                    else Nothing
    mp' = Set.insert (x', y') mp
    x' = x + dx
    y' = y + dy

strToMove :: String -> (Bool, Int)
strToMove (h:rest) = (h == 'L', read rest)

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
