{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use second" #-}
module Config.ConfigValidator (module Config.ConfigValidator) where
import Config.ConfigTypes
import qualified Data.Map as Map
import Data.Foldable
import Config.ConfigParser

possibleTableNames :: [String]
possibleTableNames = ["debug", "parsing", "testing"]

debugOptions :: [(String, ConfigTypeName)]
debugOptions = [("symbol-table", BoolT), ("function-symbol-table", BoolT), ("context", BoolT)]

parsingOptions :: [(String, ConfigTypeName)]
parsingOptions = [("collapse-operations", BoolT), ("collapse-control-flow", BoolT)]

testingOptions :: [(String, ConfigTypeName)]
testingOptions = [("colors", BoolT)]

getOptions :: Table -> Options
getOptions (Table _ opts) = opts

getName :: Table -> String
getName (Table name _) = name

validateTables :: ParsedConfig -> Bool
validateTables = all validateTable
    where
        validateTable table =
            validateName table && validateOptions table


validateName :: Table -> Bool
validateName table = elem (getName table) possibleTableNames

checkOptions :: [(String, ConfigTypeName)] -> [(String, ConfigTypeName)] -> Bool
checkOptions options expected = all (checkOption expected) options
    where
        checkOption expected option = elem option expected
    

validateOptions :: Table -> Bool
validateOptions table =
    case getName table of
        "debug" -> checkOptions options debugOptions
        "parsing" -> checkOptions options parsingOptions
        "testing" -> checkOptions options testingOptions
    where
        options =
            map
                (\(k, v) -> (k, configValueToType v))
                (Map.toList $ getOptions table)


flattenConfig :: ParsedConfig -> FlatParsedConfig
flattenConfig = foldr (Map.union . getOptions) Map.empty

existsTable :: String -> ParsedConfig -> Bool
existsTable targetName =
    any (\(Table name _) -> name == targetName)
