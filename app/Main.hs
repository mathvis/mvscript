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
import Types hiding (fst)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import FunctionStorage
import Control.Exception

parseFile :: String -> String -> ParserState -> ([Statement], ParserState)
parseFile filename file state =
    case runParser parseStatementThenReturnState state filename file of
        Left e -> error ("Error while parsing: " ++ show e)
        Right parsed -> parsed
  where
    whitespaceOrNewline = skipMany (oneOf " \t\n\r;")
    parseStatementThenReturnState = do
        statements <- sepEndBy parseStatement whitespaceOrNewline
        state <- getState
        return (statements, state)

parseFileDebug :: String -> String -> ParserState -> ([(Statement, ParserState)], ParserState)
parseFileDebug filename file state =
    case runParser parseStatementsWithStateThenReturnState state filename file of
        Left e -> error ("Error while parsing: " ++ show e)
        Right parsed -> parsed
  where
    whitespaceOrNewline = skipMany (oneOf " \t\n\r;")
    
    parseStatementWithState = do
        stmt <- parseStatement
        currentState <- getState
        return (stmt, currentState)
    
    parseStatementsWithStateThenReturnState = do
        statements <- sepEndBy parseStatementWithState whitespaceOrNewline
        finalState <- getState
        return (statements, finalState)

parseConfig :: String -> [Table]
parseConfig config =
    do
        case runParser (many parseTable) defaultParserState "config" config of
            Left e -> error ("Error while parsing: " ++ show e)
            Right parsed -> parsed

main :: IO ()
main =
    do
        home <- getHomeDirectory
        (filename : flags) <- getArgs
        configFile <- readFile (fromMaybe (home </> ".mvscc/config.toml") (listToMaybe flags))
        fileContents <- readFile filename
        let state = setConfig defaultParserState (parseConfig configFile)
        let parsed = if debug (config state)
            then Debug (parseFileDebug filename fileContents state)
            else NoDebug (parseFile filename fileContents state)
        case parsed of
            Debug debugParsed -> do
                evaluate $ resolveFunctionCalls (snd debugParsed)
                mapM_ print $ fst debugParsed
            NoDebug normalParsed -> do
                evaluate $ resolveFunctionCalls (snd normalParsed)
                mapM_ print $ fst normalParsed
            
