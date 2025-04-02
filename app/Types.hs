module Types where

import Data.Text as T
import Data.Map 

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
    | VarIdentifier String
    | FunctionIdentifier String
    | FunctionCall Expression [Expression]
    | LambdaFunc [(Expression, TypeName)] (Maybe Statement) 
    | LambdaApplication Expression Expression
    deriving (Eq, Show)

data Declaration
    = Variable Expression (Maybe TypeName) (Maybe Expression)
    | Assignment Operation
    | FunctionDef Expression [(Expression, TypeName)] TypeName (Maybe Statement)
    deriving (Eq, Show)

data BlockType = NoType | If | Else | FunctionBlock deriving (Eq, Show)

data Statement = Decl Declaration | Expr Expression | Comment String | Block BlockType [Statement] deriving (Eq, Show)

reservedKeywords :: [String]
reservedKeywords = ["if", "else", "let", "return", "Vector", "Point", "Matrix","true", "false", "func"]
