module Lib where

import Control.Exception
import Data.Char
import Debug.Trace
import System.Exit
import System.IO

type Pair a = (a, a)

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
        (try . evaluate . readFile) path :: IO (Either IOException (IO String))
    case file_cont_ex of
        Left exc -> do
            putStrLn ("Error:" ++ show exc ++ "wile reading file:" ++ path)
            exit 1
        Right textIO -> do
            textIO

exit :: Int -> IO a
exit = exitWith . ExitFailure

(|>) :: a -> (a -> b) -> b
x |> f = f x

infixl 1 |>

(*$*) :: (a -> a', b -> b') -> (a, b) -> (a', b')
(f, f') *$* (x, x') = (f x, f' x')

infixr 0 *$*

boths :: (a -> b -> c) -> Pair a -> Pair b -> Pair c
boths f (a, b) (a', b') = (f a a', f b b')

($*) :: (a -> a') -> Pair a -> Pair a'
f $* (a, a') = (f a, f a')

infixr 0 $*

(*$) :: (a -> b, a -> b') -> a -> (b, b')
(f, f') *$ a = (f a, f' a)

infixr 0 *$

setAt :: Int -> a -> [a] -> [a]
setAt idx val arr = take idx arr ++ [val] ++ drop (idx + 1) arr

(=:>) :: (a -> b) -> (b -> c) -> a -> c
f =:> g = \x -> g (f x)

infixl 9 =:>

sLogId :: Show a => String -> a -> a
sLogId msg val = trace (msg ++ ": " ++ show val) val

logId :: Show a => a -> a
logId val = trace (show val) val

pStr :: String -> IO ()
pStr = hPutStr stdout

pStr' :: String -> IO ()
pStr' = hPutStr stderr

block :: String -> String -> IO ()
block title content = do
    pStr' $ title ++ ":\n"
    pStr' "----------------\n"
    pStr' $ content ++ "\n"
    pStr' "----------------\n"

infinity :: Int
infinity = 9223372036854775807
