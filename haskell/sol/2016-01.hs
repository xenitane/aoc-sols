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

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

type Point = Pair Int

solve :: String -> String
solve input = pairToStr (first, second)
  where
    first = abs x + abs y
    second = abs x' + abs y'
    ((x, y), Just (x', y'), _, _) =
        input |> splitOn ", " |>
        foldl rotateAndMove ((0, 0), Nothing, (0, 1), Set.empty)

rotateAndMove ::
       (Point, Maybe Point, Point, Set Point)
    -> String
    -> (Point, Maybe Point, Point, Set Point)
rotateAndMove state (dir:steps) =
    state |> rotate (dir == 'L') |> moveN (read steps)

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
    let x' = x + dx
        y' = y + dy
        mp' = Set.insert (x', y') mp
        act' =
            case act of
                Just p -> act
                _
                    | Set.member (x', y') mp -> Just (x', y')
                _ -> Nothing
     in moveN (n - 1) ((x', y'), act', (dx, dy), mp')

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
