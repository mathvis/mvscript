module ConfigParser where

import Text.ParserCombinators.Parsec
import Control.Monad.State
import Misc
import ConfigTypes

parseTable :: Parser Table
parseTable = lexeme $ Table <$> (char '[' *> many letter <* string "]\n") <*> many parseOption

parseBool :: Parser ConfigType
parseBool = Bool <$> (parseTrue <|> parseFalse)
  where
    parseTrue = True <$ string "true"
    parseFalse = False <$ string "false"

parseInt :: Parser ConfigType
parseInt = Int . read <$> many digit

parseString :: Parser ConfigType
parseString = String <$> (char '"' *> many (noneOf "\"")<* char '"')

parseOption :: Parser (String, ConfigType)
parseOption = (,) <$> lexeme (many letter) <* lexeme (char '=') <*> lexeme parseBool
