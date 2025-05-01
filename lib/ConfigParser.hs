module ConfigParser where

import Text.ParserCombinators.Parsec
import Control.Monad.State
import Misc
import ConfigTypes
import Types (MVParser)

parseTable :: MVParser Table
parseTable = lexeme $ Table <$> (char '[' *> many letter <* string "]\n") <*> many parseOption

parseBool :: MVParser ConfigType
parseBool = Bool <$> (parseTrue <|> parseFalse)
  where
    parseTrue = True <$ string "true"
    parseFalse = False <$ string "false"

parseInt :: MVParser ConfigType
parseInt = Int . read <$> many digit

parseString :: MVParser ConfigType
parseString = String <$> (char '"' *> many (noneOf "\"")<* char '"')

parseOption :: MVParser (String, ConfigType)
parseOption = (,) <$> lexeme (many letter) <* lexeme (char '=') <*> lexeme parseBool
