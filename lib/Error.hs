module Error (module Error) where

import GHC.IO.Unsafe
import System.Exit
import Text.Parsec
import Types
import qualified Data.Map as Map
import qualified Config.ConfigTypes

padF :: (Int -> Int) -> Int -> String
padF _ 0 = "" 
padF f n = " " ++ padF f (f n)

padDigits :: Int -> String 
padDigits = padF (`div` 10) 

countTabs :: String -> Int
countTabs = length . filter (== '\t')

pad :: Int -> String
pad = padF (\n -> n - 1)

bold :: ParserState -> String -> String
bold state str =
    if getColors state
        then boldString
        else str
    where
        boldString = "\ESC[1m" ++ str ++ "\ESC[0m"

darkerBlue :: ParserState -> String -> String
darkerBlue state str =
    if getColors state
        then colorString
        else str
    where
        colorString = "\ESC[38;2;107;141;196m" ++ str ++ "\ESC[0m" 

blue :: ParserState -> String -> String
blue state str =
    if getColors state
        then colorString
        else str
    where
        colorString = "\ESC[38;2;137;180;250m" ++ str ++ "\ESC[0m" 

lightBlue :: ParserState -> String -> String
lightBlue state str =
    if getColors state
        then colorString
        else str
    where
        colorString = "\ESC[38;2;148;226;213m" ++ str ++ "\ESC[0m"

yellow :: ParserState -> String -> String
yellow state str =
    if getColors state
        then colorString
        else str
    where
        colorString = "\ESC[38;2;232;209;161m" ++ str ++ "\ESC[0m"

red :: ParserState -> String -> String
red state str =
    if getColors state
        then colorString
        else str
    where
        colorString = "\ESC[38;2;243;139;168m" ++ str ++ "\ESC[0m"


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

error :: SourcePos -> ParserState -> String -> String -> a
error pos state msg hint = unsafePerformIO $ do
    line <- getErrorLine pos
    putStrLn $ bold state (red state "error") ++ bold state ": " ++ bold state msg
    putStrLn $ padDigits (sourceLine pos) ++ blue state "--> " ++ sourceName pos ++ ":" ++ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
    putStrLn $ padDigits (sourceLine pos) ++ blue state " |"
    putStrLn $ blue state (show (sourceLine pos) ++ " | ") ++ line 
    putStrLn $ padDigits (sourceLine pos) ++ blue state " |" ++ pad (sourceColumn pos - 4 * countTabs line) ++ red state "^"
    putStrLn $ padDigits (sourceLine pos) ++ blue state " |"
    putStrLn $ bold state (lightBlue state "hint") ++ ": " ++ hint
    exitFailure

getColors :: ParserState -> Bool
getColors state = case Map.lookup "colors" (config state) of
    Just (Config.ConfigTypes.Bool bool) -> bool
    Just _ -> Prelude.error "I did something wrong."
    Nothing -> Prelude.error "I did something wrong again."

