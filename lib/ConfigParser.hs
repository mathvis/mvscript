module ConfigParser where

import Text.ParserCombinators.Parsec
import Control.Monad.State
import Misc
import ConfigTypes
import Types (MVParser)
import qualified Data.Map as Map

parseTable :: MVParser Table
parseTable = lexeme $ Table <$> (char '[' *> many letter <* string "]\n") <*> (Map.fromList <$> many parseOption)

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
parseOption = (,) <$> lexeme (many (letter <|> char '-')) <* lexeme (char '=') <*> lexeme parseBool

