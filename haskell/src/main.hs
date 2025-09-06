{-# LANGUAGE CPP #-}

module Main where

import Lib
import Opts
import Sol

main :: IO ()
#if !defined TEST_MODE
main = do
    input <- safeReadFile input_file_path
    input |> solve |> pStr
#else
main = do
    input <- safeReadFile test_input_file_path
    expected <- trimTrailing <$> safeReadFile test_output_file_path
    let actual = input |> solve |> trimTrailing
     in if actual == expected
            then pStr' "test passed\n"
            else do
                pStr' "test failed\n"
                block "Expected" expected
                block "Actual" actual
                exit 1
#endif
