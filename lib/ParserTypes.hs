module ParserTypes (module ParserTypes) where

import Data.Text as T hiding (map, show)

import Prelude hiding (fst)
import Text.Megaparsec hiding (State)


data ParserType
    = StringT
    | IntT
    | FloatT
    | BoolT
    | VectorT
    | PointT
    | MatrixT
    | ArrayT ParserType
    | VoidT
    | LambdaT [ParserType] ParserType
    deriving Eq

instance Show ParserType where
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
    deriving (Show, Eq)

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
    deriving (Show, Eq)

data Expression
    = Literal SourcePos Literal
    | Parentheses SourcePos Expression
    | Operation SourcePos Operation
    | Identifier SourcePos T.Text
    | FunctionCall SourcePos Expression [Expression]
    | LambdaFunc SourcePos [(Expression, ParserType)] TopLevel
    | LambdaApplication SourcePos Expression Expression
    deriving Show

instance Eq Expression where
    Literal _ a == Literal _ b = a == b
    Parentheses _ a == Parentheses _ b = a == b
    Operation _ a == Operation _ b = a == b
    Identifier _ a == Identifier _ b = a == b
    FunctionCall _ f1 a1 == FunctionCall _ f2 a2 = f1 == f2 && a1 == a2
    LambdaFunc _ a b == LambdaFunc _ c d = a == c && b == d
    LambdaApplication _ a b == LambdaApplication _ c d = a == c && b == d
    _ == _ = False
    


data Statement
    = Variable SourcePos Expression (Maybe ParserType) (Maybe Expression)
    | Constant SourcePos Expression ParserType Expression
    | Assignment SourcePos Operation
    | FunctionDef SourcePos Expression [(Expression, ParserType)] ParserType (Maybe TopLevel)
    | IfStmt SourcePos (Maybe Expression) TopLevel (Maybe TopLevel)
    | ElseStmt SourcePos TopLevel
    | Return SourcePos (Maybe Expression)
    deriving Show

instance Eq Statement where
    Variable _ a1 b1 c1 == Variable _ a2 b2 c2 = a1 == a2 && b1 == b2 && c1 == c2
    Constant _ a1 b1 c1 == Constant _ a2 b2 c2 = a1 == a2 && b1 == b2 && c1 == c2 
    Assignment _ a == Assignment _ b = a == b
    FunctionDef _ a1 b1 c1 d1 == FunctionDef _ a2 b2 c2 d2 = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2
    IfStmt _ a1 b1 c1 == IfStmt _ a2 b2 c2 = a1 == a2 && b1 == b2 && c1 == c2
    ElseStmt _ a == ElseStmt _ b = a == b
    Return _ a == Return _ b = a == b
    _ == _ = False


data BlockType = NoType | If | Else | FunctionBlock ParserType deriving (Show, Eq)

data TopLevel = Stmt Statement | Expr Expression | Block BlockType [TopLevel] deriving (Show, Eq)

reservedKeywords :: [String]
reservedKeywords = ["if", "else", "let", "return", "Vector", "Point", "Matrix","true", "false", "func", "const", "fwd", "int", "bool", "float", "string", "vector", "point", "matrix"]

newtype MVParseError
  = ReservedKeywordUsed String
  deriving (Show, Eq, Ord)

instance ShowErrorComponent MVParseError where
  showErrorComponent (ReservedKeywordUsed name) =
    "cannot use reserved keyword '" ++ name ++ "' as an identifier"

type MVParser = Parsec MVParseError String
