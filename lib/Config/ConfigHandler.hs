{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module Config.ConfigHandler (module Config.ConfigHandler) where
import Types hiding (Bool)
import Config.ConfigTypes
import qualified Data.Map as Map
import Data.List
import Config.ConfigValidator
import Config.ConfigParser
import PrettyPrint
import Error
import Control.Monad

setConfig :: ParserState -> ParsedConfig -> ParserState
setConfig state config = state{config=Map.union flatConfig defaultConfig}
    where
        flatConfig = flattenConfig config

getEnabledDebugOptions :: ParserState -> FlatParsedConfig
getEnabledDebugOptions state = Map.filterWithKey isTrueDebug (config state)
    where
        isTrueDebug name (Bool bool) =
            elem name (map Prelude.fst debugOptions) && bool

printWithDebugOptions :: (Statement, ParserState) -> IO ()
printWithDebugOptions statefulStmt@(stmt, state) =
    do
        print stmt
        unless (Map.null options) $ do
                putStrLn $ (bold state . red state) "\n--- STATE ---\n"
                mapM_ (printOptionIfEnabled state) (Map.toList options)
    where
        options = getEnabledDebugOptions state

printOptionIfEnabled :: ParserState -> (String, ConfigType) -> IO ()
printOptionIfEnabled state (name, _) =
    case name of
        "symbol-table" -> pprint (st state) state
        "function-symbol-table" -> pprint (Types.fst state) state
        "context" -> pprint (context state) state
        _ -> Prelude.error "you forgot to add the option."

getBoolOption :: String -> ParserState -> Bool
getBoolOption name state = case Map.lookup name (config state) of
    Just (Config.ConfigTypes.Bool bool) -> bool
    Just _ -> Prelude.error "I did something wrong."
    Nothing -> Prelude.error "I did something wrong again."


getCollapseControlFlow :: ParserState -> Bool
getCollapseControlFlow = getBoolOption "collapse-control-flow"

getCollapseOperations :: ParserState -> Bool
getCollapseOperations = getBoolOption "collapse-operations"
