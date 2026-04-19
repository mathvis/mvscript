module Main where

import Config.ConfigValidator
import Config.ConfigParser
import Config.ConfigTypes
import Data.List
import Data.Map
import qualified Data.Map as Map
import Data.Maybe
import Parser
import System.Environment
import System.Exit
import Types hiding (fst)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.State
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import FunctionStorage
import Control.Exception hiding (try)
import Config.ConfigHandler
import Misc

parseFileDebug :: String -> String -> ParserState -> ([(Statement, ParserState)], ParserState)
parseFileDebug filename file state =
    case runState (runParserT parseStatementsWithStateThenReturnState filename file) state of
        (Left e, _) -> error ("Error while parsing: " ++ errorBundlePretty e)
        (Right parsed, finalState) -> (parsed, finalState)
    where
        parseStatementWithState = do
            stmt <- parseStatement
            currentState <- get
            return (stmt, currentState)
        parseStatementsWithStateThenReturnState =
            sc *> many parseStatementWithState <* eof

parseConfig :: String -> [Table]
parseConfig config =
    case evalState (runParserT (many parseTable) "config" config) defaultParserState of
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
        let parsed = parseFileDebug filename fileContents state
        evaluate $ resolveFunctionCalls (snd parsed)
        mapM_ printWithDebugOptions $ fst parsed
            
