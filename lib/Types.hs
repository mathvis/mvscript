module Types (module Types) where

import Data.Text as T hiding (map, show)
import Data.Map hiding (map)
import qualified Data.Map as Map hiding (map)
import Prelude hiding (fst)
import Config.ConfigTypes
import Text.Megaparsec hiding (State)
import Data.Void
import Control.Monad.State

data TypeName
    = StringT
    | IntT
    | FloatT
    | BoolT
    | VectorT
    | PointT
    | MatrixT
    | ArrayT TypeName
    | VoidT
    | LambdaT [TypeName] TypeName
    deriving Eq

instance Show TypeName where
    show typename = case typename of
        Types.StringT -> "string"        
        Types.IntT -> "int"        
        FloatT -> "float"        
        Types.BoolT -> "bool"        
        VectorT -> "vector"        
        PointT -> "point"        
        MatrixT -> "matrix"        
        ArrayT typ -> "[" ++ show typ ++ "]"        
        VoidT -> "void"
        LambdaT args ret -> "lambda[" ++ T.unpack (intercalate (T.pack ", ") (map (T.pack . show) args)) ++ "]" ++ show ret


data Type
    = String T.Text
    | Int Integer
    | Float Float
    | Bool Bool
    | Array [Expression]
    | Vector [Expression]
    | Point [Expression]
    | Matrix [Expression]
    deriving (Eq, Show)

data Operation
    -- BINARY ARITHMETIC
    = Add Expression Expression
    | Subtract Expression Expression
    | Multiply Expression Expression
    | IntDivide Expression Expression
    | Divide Expression Expression
    | Modulo Expression Expression
    -- UNARY ARITHMETIC
    | Negation Expression
    -- COMPARISON OPERATORS
    | GreaterThan Expression Expression
    | LessThan Expression Expression
    | GreaterThanEq Expression Expression
    | LessThanEq Expression Expression
    | Equals Expression Expression
    | NotEquals Expression Expression
    -- LOGIC OPERATORS
    | Or Expression Expression
    | And Expression Expression
    | Not Expression
    | BitwiseOr Expression Expression
    | BitwiseAnd Expression Expression
    | BitwiseXor Expression Expression
    | BitwiseNot Expression
    -- ASSIGNMENT OPERATORS
    | AddAssign Expression Expression
    | SubAssign Expression Expression
    | MulAssign Expression Expression
    | DivAssign Expression Expression
    | IntDivAssign Expression Expression
    | ModAssign Expression Expression
    | BitwiseOrAssign Expression Expression
    | BitwiseAndAssign Expression Expression
    | BitwiseXorAssign Expression Expression
    | Assign Expression Expression
    deriving (Eq, Show)

data Expression
    = Type Type
    | Parentheses Expression
    | Operation Operation
    | VarIdentifier T.Text
    | FunctionIdentifier T.Text
    | FunctionCall Expression [Expression]
    | LambdaFunc [(Expression, TypeName)] (Maybe TopLevel) 
    | LambdaApplication Expression Expression
    deriving (Eq, Show)

data Statement
    = Variable Expression (Maybe TypeName) (Maybe Expression)
    | Constant Expression TypeName Expression
    | Assignment Operation
    | FunctionDef Expression [(Expression, TypeName)] TypeName (Maybe TopLevel)
    | IfBlock Expression TopLevel (Maybe Statement)
    | ElseBlock TopLevel
    | Return (Maybe Expression)
    | CollapsedControlFlow TopLevel
    deriving (Eq, Show)

data BlockType = NoType | If | Else | FunctionBlock TypeName deriving (Eq, Show)

data TopLevel = Stmt Statement | Expr Expression | Block BlockType [TopLevel] deriving (Eq, Show)

reservedKeywords :: [String]
reservedKeywords = ["if", "else", "let", "return", "Vector", "Point", "Matrix","true", "false", "func", "const", "fwd"]

class Data a where

data FunctionData = FunctionData {
    returnType :: TypeName,
    arguments :: [(T.Text, TypeName)],
    hasBody :: Bool 
} deriving Show

defaultFunctionData = FunctionData {
    returnType = VoidT,
    arguments = [],
    hasBody = False
}

instance Data FunctionData where

data VariableData = VariableData {
    variableType :: Maybe TypeName,
    inScope :: Bool,
    isInitialized :: Bool,
    isConstant :: Bool
} deriving Show

instance Data VariableData where

defaultVariableData :: VariableData
defaultVariableData = VariableData {
    variableType = Nothing,
    inScope = True,
    isInitialized = False,
    isConstant = False
}

type SymbolTable = Map T.Text VariableData
type FunctionSymbolTable = Map T.Text FunctionData
type ContextStack = [BlockType]

data FunctionCallData = FunctionCallData {
    identifier :: T.Text,
    pos :: SourcePos
} deriving (Show, Eq)

data ParserState = ParserState {
    config :: FlatParsedConfig,
    st :: SymbolTable,
    fst :: FunctionSymbolTable,
    context :: ContextStack,
    unresolvedFunctionCalls :: [FunctionCallData]
} deriving Show

defaultConfig :: FlatParsedConfig
defaultConfig = Map.fromList [
    ("symbol-table", Config.ConfigTypes.Bool False),
    ("function-symbol-table", Config.ConfigTypes.Bool False),
    ("context", Config.ConfigTypes.Bool False),
    ("colors", Config.ConfigTypes.Bool True),
    ("collapse-operations", Config.ConfigTypes.Bool True),
    ("collapse-control-flow", Config.ConfigTypes.Bool True)]

defaultParserState :: ParserState
defaultParserState = ParserState {
    config = defaultConfig,
    st = Map.empty,
    fst = Map.empty,
    context = [],
    unresolvedFunctionCalls = []
}

type MVParser = ParsecT Void String (State ParserState) 

getConfig :: MVParser FlatParsedConfig
getConfig = gets config 

getSymbolTable :: MVParser SymbolTable
getSymbolTable = gets st

getFunctionSymbolTable :: MVParser FunctionSymbolTable
getFunctionSymbolTable = gets fst

defaultSourcePos :: SourcePos
defaultSourcePos = initialPos "internal"

