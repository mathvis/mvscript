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
import Config

parseFile :: String -> [Table] -> [Statement]
parseFile file configTables =
    do
        case runParser (many parseStatement) (setConfig defaultParserState configTables) "MVScript" file of
            Left e -> error ("Error while parsing: " ++ show e)
            Right parsed -> parsed

parseConfig :: String -> [Table]
parseConfig config =
    do
        case runParser (many parseTable) defaultParserState "config" config of
            Left e -> error ("Error while parsing: " ++ show e)
            Right parsed -> parsed

main :: IO ()
main =
    do
        (filename:flags) <- getArgs
        config <- readFile "config.toml"
        let configTables = parseConfig config
        fileContents <- readFile filename
        mapM_ print $ parseFile fileContents configTables
        
