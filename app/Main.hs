module Main where

import Config
import ConfigParser
import ConfigTypes
import Data.List
import Data.Map
import qualified Data.Map as Map
import Data.Maybe
import Parser
import System.Environment
import System.Exit
import Text.ParserCombinators.Parsec
import Types

parseFile :: String -> String -> ParserState -> [Statement]
parseFile filename file state =
    do
        case runParser (many parseStatement) state filename file of
            Left e -> error ("Error while parsing: " ++ show e)
            Right parsed -> parsed

parseFileDebug :: String -> String -> ParserState -> [(Statement, ParserState)]
parseFileDebug filename file state =
    case runParser parseStatementsState state filename file of
        Left e -> error ("Error while parsing: " ++ show e)
        Right parsed -> parsed
  where
    parseStatementsState :: MVParser [(Statement, ParserState)]
    parseStatementsState = do
        option [] $ do
            stmt <- parseStatement
            currentState <- getState
            rest <- parseStatementsState
            return ((stmt, currentState) : rest)

parseConfig :: String -> [Table]
parseConfig config =
    do
        case runParser (many parseTable) defaultParserState "config" config of
            Left e -> error ("Error while parsing: " ++ show e)
            Right parsed -> parsed

main :: IO ()
main =
    do
        (filename : flags) <- getArgs
        configFile <- readFile (fromMaybe "config.toml" (listToMaybe flags))
        fileContents <- readFile filename
        let state = setConfig defaultParserState (parseConfig configFile)
        if debug (config state)
            then mapM_ print $ parseFileDebug filename fileContents state
            else mapM_ print $ parseFile filename fileContents state
