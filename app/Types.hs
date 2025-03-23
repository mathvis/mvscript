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
    = -- BINARY ARITHMETIC
      Add Expression Expression
    | Subtract Expression Expression
    | Multiply Expression Expression
    | IntDivide Expression Expression
    | Divide Expression Expression
    | Modulo Expression Expression
    | -- UNARY ARITHMETIC
      Negation Expression
    | -- COMPARISON OPERATORS
      GreaterThan Expression Expression
    | LessThan Expression Expression
    | GreaterThanEq Expression Expression
    | LessThanEq Expression Expression
    | Equals Expression Expression
    | NotEquals Expression Expression
    | -- LOGIC OPERATORS
      Or Expression Expression
    | And Expression Expression
    | Not Expression
    | BitwiseOr Expression Expression
    | BitwiseAnd Expression Expression
    | BitwiseXor Expression Expression
    | BitwiseNot Expression
    | -- ASSIGNMENT OPERATORS
      AddAssign Expression Expression
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
    deriving (Eq, Show)

data Declaration
    = Variable Expression (Maybe TypeName) (Maybe Expression)
    | Assignment Expression
    deriving (Eq, Show)

data Statement = Decl Declaration | Expr Expression | Comment String deriving (Eq, Show)

