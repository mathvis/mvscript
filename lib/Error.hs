module Error where

import GHC.IO.Unsafe
import System.Exit
import Text.Parsec
import Types

padF :: (Int -> Int) -> Int -> String
padF _ 0 = "" 
padF f n = " " ++ padF f (f n)

padDigits :: Int -> String 
padDigits = padF (`div` 10) 

countTabs :: String -> Int
countTabs = length . filter (== '\t')

pad :: Int -> String
pad = padF (\n -> n - 1)

bold :: String -> String
bold str = "\ESC[1m" ++ str ++ "\ESC[0m"

blue :: String -> String
blue str = "\ESC[38;2;137;180;250m" ++ str ++ "\ESC[0m" 

red :: String -> String
red str = "\ESC[38;2;243;139;168m" ++ str ++ "\ESC[0m"


getErrorLine :: SourcePos -> IO String
getErrorLine pos = do
    contents <- lines <$> readFile (sourceName pos)
    return $ contents !! (sourceLine pos - 1)
    

simpleError :: SourcePos -> String -> a
simpleError pos msg = unsafePerformIO $ do
    line <- getErrorLine pos
    putStrLn $ "error: " ++ msg
    putStrLn $ padDigits (sourceLine pos) ++ "--> " ++ sourceName pos ++ ":" ++ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
    putStrLn $ padDigits (sourceLine pos) ++ " |"
    putStrLn $ show (sourceLine pos) ++ " | " ++ line 
    putStrLn $ padDigits (sourceLine pos) ++ " |" ++ pad (sourceColumn pos - 4 * countTabs line) ++ "^"
    putStrLn $ padDigits (sourceLine pos) ++ " |"
    exitFailure

colorizedError :: SourcePos -> String -> a
colorizedError pos msg = unsafePerformIO $ do
    line <- getErrorLine pos
    putStrLn $ bold (red "error") ++ bold ": " ++ bold msg
    putStrLn $ padDigits (sourceLine pos) ++ blue "--> " ++ sourceName pos ++ ":" ++ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
    putStrLn $ padDigits (sourceLine pos) ++ blue " |"
    putStrLn $ blue (show (sourceLine pos) ++ " | ") ++ line 
    putStrLn $ padDigits (sourceLine pos) ++ blue " |" ++ pad (sourceColumn pos - 4 * countTabs line) ++ red "^"
    putStrLn $ padDigits (sourceLine pos) ++ blue " |"
    exitFailure


error :: ParserState -> SourcePos -> String -> a
error state pos msg =
    if colors (config state) then
        colorizedError pos msg
    else
        simpleError pos msg
