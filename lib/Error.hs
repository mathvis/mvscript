module Error where

import GHC.IO.Unsafe
import System.Exit
import Text.Parsec
import Types

bold :: String -> String
bold str = "\ESC[1m" ++ str ++ "\ESC[0m"

blue :: String -> String
blue str = "\ESC[38;2;137;180;250m" ++ str ++ "\ESC[0m" 

red :: String -> String
red str = "\ESC[38;2;243;139;168m" ++ str ++ "\ESC[0m"

simpleError :: SourcePos -> String -> a
simpleError pos msg = unsafePerformIO $ do
    putStrLn $ "--> " ++ sourceName pos ++ ":" ++ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
    putStrLn $ "error: " ++ msg
    exitFailure

colorizedError :: SourcePos -> String -> a
colorizedError pos msg = unsafePerformIO $ do
    putStrLn $ blue "--> " ++ sourceName pos ++ ":" ++ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
    putStrLn $ red "error" ++ ": " ++ msg
    exitFailure


error :: ParserState -> SourcePos -> String -> a
error state pos msg =
    if colors (config state) then
        colorizedError pos msg
    else
        simpleError pos msg
