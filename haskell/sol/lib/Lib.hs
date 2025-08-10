module Lib where

import Control.Exception (IOException, evaluate, try)
import Data.Char (isSpace)
import System.Exit (ExitCode(ExitFailure), exitWith)

class Stringy a where
    stringy :: a -> String

instance Stringy () where
    stringy :: () -> String
    stringy _ = ""

instance Stringy String where
    stringy :: String -> String
    stringy s = s ++ "\n"

instance Stringy Int where
    stringy :: Int -> String
    stringy x = show x ++ "\n"

pairToStr :: (Stringy a1, Stringy a2) => (a1, a2) -> String
pairToStr (first, second) = stringy first ++ stringy second

trimTrailing :: String -> String
trimTrailing = reverse . dropWhile isSpace . reverse

safeReadFile :: FilePath -> IO String
safeReadFile path = do
    file_cont_ex <-
        try $ evaluate $ readFile path :: IO (Either IOException (IO String))
    case file_cont_ex of
        Left exc -> do
            putStrLn $ "Error:" ++ show exc ++ "wile reading file:" ++ path
            exitWith $ ExitFailure 1
        Right textIO -> do
            textIO
