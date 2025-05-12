module Types where

import Data.Text as T
import Text.ParserCombinators.Parsec
import Data.Map
import Text.Parsec
import qualified Data.Map as Map
import Prelude hiding (fst)
import Text.Parsec.Pos

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
    deriving (Eq, Show)

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
    | LambdaFunc [(Expression, TypeName)] (Maybe Statement) 
    | LambdaApplication Expression Expression
    deriving (Eq, Show)

data Declaration
    = Variable Expression (Maybe TypeName) (Maybe Expression)
    | Constant Expression TypeName Expression
    | Assignment Operation
    | FunctionDef Expression [(Expression, TypeName)] TypeName (Maybe Statement)
    | IfBlock Expression Statement (Maybe Declaration)
    | ElseBlock Statement
    | CollapsedControlFlow Statement
    deriving (Eq, Show)

data BlockType = NoType | If | Else | FunctionBlock deriving (Eq, Show)

data Statement = Decl Declaration | Expr Expression | Comment String | Block BlockType [Statement] deriving (Eq, Show)

reservedKeywords :: [String]
reservedKeywords = ["if", "else", "let", "return", "Vector", "Point", "Matrix","true", "false", "func", "const"]

data Configuration = Configuration {
    debug :: Bool,
    collapseOperations :: Bool,
    collapseControlFlow :: Bool,
    colors :: Bool
} deriving Show

defaultConfig :: Configuration
defaultConfig = Configuration {
    debug = False,
    collapseOperations = False,
    collapseControlFlow = True,
    colors = True
}

data FunctionData = FunctionData {
    returnType :: Maybe TypeName,
    arguments :: [(T.Text, TypeName)],
    functionBody :: [Statement]
} deriving Show

data VariableData = VariableData {
    variableType :: Maybe TypeName,
    inScope :: Bool,
    isInitialized :: Bool,
    isConstant :: Bool
} deriving Show

defaultVariableData :: VariableData
defaultVariableData = VariableData {
    variableType = Nothing,
    inScope = True,
    isInitialized = False,
    isConstant = False
}

type SymbolTable = Map T.Text VariableData
type FunctionSymbolTable = Map T.Text FunctionData

data ParserState = ParserState {
    config :: Configuration,
    st :: SymbolTable,
    fst :: FunctionSymbolTable
} deriving Show

defaultParserState :: ParserState
defaultParserState = ParserState {
    config = defaultConfig,
    st = Map.empty,
    fst = Map.empty
}

type MVParser a = Parsec String ParserState a

getConfig :: MVParser Configuration
getConfig = config <$> getState

getSymbolTable :: MVParser SymbolTable
getSymbolTable = st <$> getState

getFunctionSymbolTable :: MVParser FunctionSymbolTable
getFunctionSymbolTable = fst <$> getState

defaultSourcePos :: SourcePos
defaultSourcePos = newPos "internal" 0 0
