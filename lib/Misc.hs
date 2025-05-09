module Misc where
import Text.ParserCombinators.Parsec
import Control.Monad
import Types

whitespace :: MVParser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: MVParser a -> MVParser a
lexeme p = p <* whitespace

endLine :: MVParser a -> MVParser a
endLine p = p <* char ';'

newLine :: MVParser a -> MVParser a
newLine p = p <* optional (char '\n')

betweenParentheses :: MVParser a -> MVParser a
betweenParentheses = between (lexeme $ char '(') (lexeme $ char ')') 

rword :: String -> MVParser ()
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

