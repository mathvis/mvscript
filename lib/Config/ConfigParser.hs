module Config.ConfigParser (module Config.ConfigParser) where

import Misc
import Config.ConfigTypes
import Types (MVParser)
import qualified Data.Map as Map
import Text.Megaparsec
import Text.Megaparsec.Char

parseTable :: MVParser Table
parseTable = lexeme $ Table <$> (char '[' *> many letterChar <* string "]\n") <*> (Map.fromList <$> many parseOption)

parseBool :: MVParser ConfigType
parseBool = Bool <$> (parseTrue <|> parseFalse)
  where
    parseTrue = True <$ string "true"
    parseFalse = False <$ string "false"

parseInt :: MVParser ConfigType
parseInt = Int . read <$> many digitChar

parseString :: MVParser ConfigType
parseString = String <$> (char '"' *> many (noneOf "\"")<* char '"')

parseOption :: MVParser (String, ConfigType)
parseOption = (,) <$> lexeme (many (letterChar <|> char '-')) <* lexeme (char '=') <*> lexeme parseBool

configValueToType :: ConfigType -> ConfigTypeName
configValueToType (String _) = StringT
configValueToType (Int _) = IntT
configValueToType (Bool _) = BoolT
  
