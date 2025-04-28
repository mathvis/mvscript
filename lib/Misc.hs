module Misc where
import Text.ParserCombinators.Parsec
import Control.Monad

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

endLine :: Parser a -> Parser a
endLine p = p <* char ';'

newLine :: Parser a -> Parser a
newLine p = p <* optional (char '\n')

betweenParentheses :: Parser a -> Parser a
betweenParentheses = between (lexeme $ char '(') (lexeme $ char ')') 

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy (letter <|> digit))

toInt' :: (Num a, Real a) => a -> Integer
toInt' = round . realToFrac

intDiv :: (Num a, Real a) => a -> a -> Integer
intDiv x y = div (toInt' x) (toInt' y)

realToBool :: Real a => a -> Bool
realToBool 0 = False
realToBool _ = True

boolToInt :: Bool -> Integer
boolToInt True = 1
boolToInt False = 0
