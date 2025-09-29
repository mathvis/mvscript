module Misc (module Misc) where
import Text.ParserCombinators.Parsec
import Control.Monad
import Types
import System.Exit
import System.IO.Unsafe
import qualified Data.Text as T
import Data.Map as Map hiding (map)
import Error
import Prelude hiding (fst, error)

whitespace :: MVParser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: MVParser a -> MVParser a
lexeme p = p <* whitespace

endLine :: MVParser a -> MVParser a
endLine p = p <* optional (char ';') 

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

valueToType :: Type -> TypeName
valueToType (String _) = StringT
valueToType (Int _) = IntT
valueToType (Float _) = FloatT
valueToType (Bool _) = BoolT
valueToType (Vector _) = VectorT
valueToType (Point _) = PointT
valueToType (Matrix _) = MatrixT
valueToType (Array ((Type a):_)) = ArrayT (valueToType a)

getVariableType :: SourcePos -> ParserState -> T.Text -> TypeName
getVariableType pos state name = case Map.lookup name (st state) of
    Just vData -> case variableType vData of
        Just typ -> typ
        Nothing -> error pos state "Variable does not have a type." "Internal error."
    Nothing -> error pos state "Variable not found." "Internal error."

getFunctionReturnType :: SourcePos -> ParserState -> T.Text -> TypeName
getFunctionReturnType pos state name = case Map.lookup name (fst state) of
    Just fData -> returnType fData 
    Nothing -> error pos state "Variable not found." "Internal error."

getFunctionArgTypes :: SourcePos -> ParserState -> T.Text -> [TypeName]
getFunctionArgTypes pos state name = case Map.lookup name (fst state) of
    Just fData -> map snd (arguments fData) 
    Nothing -> error pos state "Variable not found." "Internal error."


intercalateStr :: String -> [String] -> String
intercalateStr delim lst = T.unpack (T.intercalate (T.pack delim) (Prelude.map T.pack lst)) 


