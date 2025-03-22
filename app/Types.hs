module Types where

import Data.Text as T

data TypeName
    = StringT
    | IntT
    | FloatT
    | BoolT
    | VectorT
    | PointT
    | MatrixT
    deriving (Eq, Show)

data Type
    = String T.Text
    | Int Integer
    | Float Float
    | Bool Bool
    | Vector [Double]
    | Point [Double]
    | Matrix [[Double]]
    deriving (Eq, Show)

data Operation
    = Add Expression Expression
    | Subtract Expression Expression
    | Multiply Expression Expression
    | IntDivide Expression Expression
    | Divide Expression Expression
    | GreaterThan Expression Expression
    | LessThan Expression Expression
    | GreaterThanEq Expression Expression
    | LessThanEq Expression Expression
    | Equals Expression Expression
    | NotEquals Expression Expression
    | Or Expression Expression
    | And Expression Expression
    | BitwiseOr Expression Expression
    | BitwiseAnd Expression Expression
    | BitwiseXor Expression Expression
    -- TODO add NOT
    deriving (Eq, Show)


data Expression
    = Type Type
    | Parentheses Expression
    | Operation Operation
    | VarIdentifier String
    deriving (Eq, Show)


data Declaration
    = Variable Expression (Maybe TypeName) (Maybe Expression)
    -- TODO incomplete function type
    | Function Type Type
    deriving (Eq, Show)

data Statement = Decl Declaration | Expr Expression deriving (Eq, Show)
