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

parseFile :: String -> ParserState -> [Statement]
parseFile file state =
    do
        case runParser (many parseStatement) state "MVScript" file of
            Left e -> error ("Error while parsing: " ++ show e)
            Right parsed -> parsed

parseFileDebug :: String -> ParserState -> [(Statement, ParserState)]
parseFileDebug file state =
        case runParser parseStatementsState state "MVScript" file of
            Left e -> error ("Error while parsing: " ++ show e)
            Right parsed -> parsed
    where
        parseStatementsState :: MVParser [(Statement, ParserState)]
        parseStatementsState = do
            option [] $ do
                stmt <- parseStatement
                currentState <- getState
                rest <- parseStatementsState
                return ((stmt, currentState):rest)

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
        fileContents <- readFile filename
        let state = setConfig defaultParserState (parseConfig config)
        if getDebugValue (getTable (parseConfig config) "debug") then
            mapM_ print $ parseFileDebug fileContents state
        else
            mapM_ print $ parseFile fileContents state        
