module Error (module Error) where

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

lightBlue :: String -> String
lightBlue str = "\ESC[38;2;148;226;213m" ++ str ++ "\ESC[0m"

red :: String -> String
red str = "\ESC[38;2;243;139;168m" ++ str ++ "\ESC[0m"


getErrorLine :: SourcePos -> IO String
getErrorLine pos = do
    contents <- lines <$> readFile (sourceName pos)
    return $ contents !! (sourceLine pos - 1)
    

simpleError :: SourcePos -> String -> String -> a
simpleError pos msg hint = unsafePerformIO $ do
    line <- getErrorLine pos
    putStrLn $ "error: " ++ msg
    putStrLn $ padDigits (sourceLine pos) ++ "--> " ++ sourceName pos ++ ":" ++ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
    putStrLn $ padDigits (sourceLine pos) ++ " |"
    putStrLn $ show (sourceLine pos) ++ " | " ++ line 
    putStrLn $ padDigits (sourceLine pos) ++ " |" ++ pad (sourceColumn pos - 4 * countTabs line) ++ "^"
    putStrLn $ padDigits (sourceLine pos) ++ " |"
    putStrLn $ "hint: " ++ hint
    exitFailure

colorizedError :: SourcePos -> String -> String -> a
colorizedError pos msg hint = unsafePerformIO $ do
    line <- getErrorLine pos
    putStrLn $ bold (red "error") ++ bold ": " ++ bold msg
    putStrLn $ padDigits (sourceLine pos) ++ blue "--> " ++ sourceName pos ++ ":" ++ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
    putStrLn $ padDigits (sourceLine pos) ++ blue " |"
    putStrLn $ blue (show (sourceLine pos) ++ " | ") ++ line 
    putStrLn $ padDigits (sourceLine pos) ++ blue " |" ++ pad (sourceColumn pos - 4 * countTabs line) ++ red "^"
    putStrLn $ padDigits (sourceLine pos) ++ blue " |"
    putStrLn $ bold (lightBlue "hint") ++ ": " ++ hint
    exitFailure


error :: SourcePos -> ParserState -> String -> String -> a
error pos state msg hint =
    if colors (config state) then
        colorizedError pos msg hint
    else
        simpleError pos msg hint
