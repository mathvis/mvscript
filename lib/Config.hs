module Config where
import Types
import ConfigTypes
import qualified Data.Map as Map
import Data.List
import Prelude hiding (fst)

(|>) :: a -> (a -> b) -> b
x |> f = f x

getTable :: [Table] -> String -> Table
getTable tables targetName = case find (\(Table name _) -> name == targetName) tables of
    Just table -> table
    Nothing -> Table "not found" Map.empty

setConfig :: ParserState -> [Table] -> ParserState
setConfig parserState tables =
    parserState
    |> (\ps -> setDebug ps (getTable tables "debug"))
    |> (\ps -> setCollapseOperations ps (getTable tables "parsing"))
    |> (\ps -> setCollapseControlFlow ps (getTable tables "parsing"))
    |> (\ps -> setColors ps (getTable tables "testing"))

getDebugValue :: Table -> Bool
getDebugValue (Table "debug" opts) = case Map.lookup "enabled" opts of
    Just (ConfigTypes.Bool val) -> val
    Nothing -> debug defaultConfig
getDebugValue _ = debug defaultConfig

getCollapseOperationsValue :: Table -> Bool
getCollapseOperationsValue (Table "parsing" opts) = case Map.lookup "collapse-operations" opts of
    Just (ConfigTypes.Bool val) -> val
    Nothing -> collapseOperations defaultConfig
getCollapseOperationsValue _ = collapseOperations defaultConfig

getCollapseControlFlowValue :: Table -> Bool
getCollapseControlFlowValue (Table "parsing" opts) = case Map.lookup "collapse-control-flow" opts of
    Just (ConfigTypes.Bool val) -> val
    Nothing -> collapseControlFlow defaultConfig
getCollapseControlFlowValue _ = collapseControlFlow defaultConfig

getColorsValue :: Table -> Bool
getColorsValue (Table "testing" opts) = case Map.lookup "colors" opts of
    Just (ConfigTypes.Bool val) -> val
    Nothing -> collapseControlFlow defaultConfig
getColorsValue _ = colors defaultConfig

setCollapseOperations :: ParserState -> Table -> ParserState
setCollapseOperations (ParserState config st fst) table = ParserState (config {collapseOperations=getCollapseOperationsValue table}) st fst

setCollapseControlFlow :: ParserState -> Table -> ParserState
setCollapseControlFlow (ParserState config st fst) table = ParserState (config {collapseControlFlow=getCollapseControlFlowValue table}) st fst

setColors :: ParserState -> Table -> ParserState
setColors (ParserState config st fst) table = ParserState (config {colors=getColorsValue table}) st fst

setDebug :: ParserState -> Table -> ParserState
setDebug (ParserState config st fst) table = ParserState (config {debug= getDebugValue table}) st fst
