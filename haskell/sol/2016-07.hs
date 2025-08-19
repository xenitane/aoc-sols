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

import Data.Set (Set)
import qualified Data.Set as Set

solve :: String -> String
solve input = pairToStr (first, second)
  where
    (first, second) =
        let (f, f') =
                (=:> boolToInt) $*
                (supportsTLS False False, supportsSSL (Set.empty, Set.empty))
            g (a, a') b = (a + f b, a' + f' b)
         in input |> lines |> foldl g (0, 0)

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

supportsSSL :: (Set String, Set String) -> String -> Bool
supportsSSL _ [_, _] = False
supportsSSL (s, s') ('[':rest) = supportsSSL (s', s) rest
supportsSSL (s, s') (']':rest) = supportsSSL (s', s) rest
supportsSSL (s, s') str =
    let [b0, b1, b2] = take 3 str
        str' = drop 1 str
     in if b0 == b2 && b0 /= b1
            then Set.member [b1, b0, b1] s' ||
                 supportsSSL (Set.insert [b0, b1, b2] s, s') str'
            else supportsSSL (s, s') str'

supportsTLS :: Bool -> Bool -> String -> Bool
supportsTLS _ supernet [_, _, _] = supernet
supportsTLS False shnet ('[':rest) = supportsTLS True shnet rest
supportsTLS True shnet (']':rest) = supportsTLS False shnet rest
supportsTLS net supernet str =
    let [b0, b1, b2, b3] = take 4 str
        str' = drop 1 str
        abba = b0 == b3 && b0 /= b1 && b1 == b2
     in if net
            then not abba && supportsTLS True supernet str'
            else supportsTLS False (supernet || abba) str'

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
