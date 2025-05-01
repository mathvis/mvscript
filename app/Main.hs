module Main where
import System.Environment
import Parser
import Text.ParserCombinators.Parsec
import Types
import System.Exit
import Data.Map
import Data.List
import ConfigTypes
import ConfigParser
import qualified Data.Map as Map

parseFile :: String -> [Statement]
parseFile file =
    do
        case runParser (many parseStatement) (defaultConfig, Map.empty, Map.empty) "MVScript" file of
            Left e -> error ("Error while parsing: " ++ show e)
            Right parsed -> parsed

parseConfig :: String -> [Table]
parseConfig config =
    do
        case runParser (many parseTable) (defaultConfig, Map.empty, Map.empty) "config" config of
            Left e -> error ("Error while parsing: " ++ show e)
            Right parsed -> parsed

main :: IO ()
main =
    do
        (filename:flags) <- getArgs
        fileContents <- readFile filename
        config <- readFile "config.toml"
        mapM_ print $ parseFile fileContents
        
