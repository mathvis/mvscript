module Misc (module Misc) where
import Types
import qualified Data.Text as T
import Data.Map as Map hiding (empty, map)
import Error
import Prelude hiding (fst, error)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

sc :: MVParser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: MVParser a -> MVParser a
lexeme = L.lexeme sc

symbol :: String -> MVParser String
symbol = L.symbol sc

betweenParentheses :: MVParser a -> MVParser a
betweenParentheses = between (symbol "(") (symbol ")") 

rword :: String -> MVParser ()
rword w = lexeme (string w *> notFollowedBy (letterChar <|> digitChar <|> char '_'))

toInt' :: (Num a, Real a) => a -> Integer
toInt' x = round (realToFrac x :: Double)

intDiv :: (Num a, Real a) => a -> a -> Integer
intDiv x y = div (toInt' x) (toInt' y)

realToBool :: Real a => a -> Bool
realToBool 0 = False
realToBool _ = True

boolToInt :: Bool -> Integer
boolToInt True = 1
boolToInt False = 0

valueToType :: Literal -> Type
valueToType (String _) = StringT
valueToType (Int _) = IntT
valueToType (Float _) = FloatT
valueToType (Bool _) = BoolT
valueToType (Vector _) = VectorT
valueToType (Point _) = PointT
valueToType (Matrix _) = MatrixT
valueToType (Array ((Literal a):_)) = ArrayT (valueToType a)
valueToType (Array _) = ArrayT (valueToType (Int 0))

getVariableType :: SourcePos -> ParserState -> T.Text -> Type
getVariableType pos state name = case Map.lookup name (st state) of
    Just vData -> case variableType vData of
        Just typ -> typ
        Nothing -> error pos state "Variable does not have a type." "Internal error."
    Nothing -> error pos state "Variable not found." "Internal error."

getFunctionReturnType :: SourcePos -> ParserState -> T.Text -> Type
getFunctionReturnType pos state name = case Map.lookup name (fst state) of
    Just fData -> returnType fData 
    Nothing -> error pos state "Variable not found." "Internal error."

getFunctionArgTypes :: SourcePos -> ParserState -> T.Text -> [Type]
getFunctionArgTypes pos state name = case Map.lookup name (fst state) of
    Just fData -> map snd (arguments fData) 
    Nothing -> error pos state "Variable not found." "Internal error."


intercalateStr :: String -> [String] -> String
intercalateStr delim lst = T.unpack (T.intercalate (T.pack delim) (Prelude.map T.pack lst)) 

