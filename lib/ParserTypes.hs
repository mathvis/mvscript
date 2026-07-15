module ParserTypes (module ParserTypes) where

import Data.Text as T hiding (map, show)

import Prelude hiding (fst)
import Text.Megaparsec hiding (State)


data Type
    = StringT
    | IntT
    | FloatT
    | BoolT
    | VectorT
    | PointT
    | MatrixT
    | ArrayT Type
    | VoidT
    | LambdaT [Type] Type
    deriving Eq

instance Show Type where
    show typename = case typename of
        StringT -> "string"        
        IntT -> "int"        
        FloatT -> "float"        
        BoolT -> "bool"        
        VectorT -> "vector"        
        PointT -> "point"        
        MatrixT -> "matrix"        
        ArrayT typ -> "[" ++ show typ ++ "]"        
        VoidT -> "void"
        LambdaT args ret -> "lambda[" ++ T.unpack (intercalate (T.pack ", ") (map (T.pack . show) args)) ++ "]" ++ show ret


data Literal
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
    = Literal Literal
    | Parentheses Expression
    | Operation Operation
    | Identifier T.Text
    | FunctionIdentifier T.Text
    | FunctionCall Expression [Expression]
    | LambdaFunc [(Expression, Type)] TopLevel
    | LambdaApplication Expression Expression
    deriving (Eq, Show)

data Statement
    = Variable Expression (Maybe Type) (Maybe Expression)
    | Constant Expression Type Expression
    | Assignment Operation
    | FunctionDef Expression [(Expression, Type)] Type (Maybe TopLevel)
    | IfStmt (Maybe Expression) TopLevel (Maybe TopLevel)
    | ElseStmt TopLevel
    | Return (Maybe Expression)
    | CollapsedControlFlow TopLevel
    deriving (Eq, Show)

data BlockType = NoType | If | Else | FunctionBlock Type deriving (Eq, Show)

data TopLevel = Stmt Statement | Expr Expression | Block BlockType [TopLevel] deriving (Eq, Show)

reservedKeywords :: [String]
reservedKeywords = ["if", "else", "let", "return", "Vector", "Point", "Matrix","true", "false", "func", "const", "fwd", "int", "bool", "float", "string", "vector", "point", "matrix"]

data MVParseError
  = ReservedKeywordUsed String
  deriving (Show, Eq, Ord)

instance ShowErrorComponent MVParseError where
  showErrorComponent (ReservedKeywordUsed name) =
    "cannot use reserved keyword '" ++ name ++ "' as an identifier"

type MVParser = Parsec MVParseError String
