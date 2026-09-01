module SemanticAnalysisTypes (
    Check,
    TypedTopLevel (..),
    ResolvedExpression (..),
    TypedExpression (..),
    ResolvedStatement (..),
    TypedOperation (..),
    Env (..),
    SemanticError (..),
    ElaboratedType (..),
    ResolvedLiteral (..),
    lookupFunc,
    lookupVar,
    insertVar,
    insertFunc,
    scope,
    globalEnv,
)
where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer (Writer)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec

data ElaboratedType
    = StringT
    | IntT
    | FloatT
    | BoolT
    | VectorT
    | PointT
    | MatrixT
    | ArrayT ElaboratedType
    | VoidT
    | ErrorT
    | UnknownT
    | LambdaT [ElaboratedType] ElaboratedType
    deriving (Show, Eq)

data SemanticError = EmptyMVContainer | TypeMismatch [ElaboratedType] ElaboratedType
    deriving (Show, Eq)

data Env = Env
    { variables :: Map Text ElaboratedType,
      functions :: Map Text ([ElaboratedType], ElaboratedType),
      parent :: Maybe Env
    }

globalEnv :: Env
globalEnv = Env Map.empty Map.empty Nothing

lookupVar :: Text -> Env -> Maybe ElaboratedType
lookupVar name env =
    case Map.lookup name (variables env) of
        Just t -> Just t
        Nothing -> parent env >>= lookupVar name

lookupFunc :: Text -> Env -> Maybe ([ElaboratedType], ElaboratedType)
lookupFunc name env =
    case Map.lookup name (functions env) of
        Just sig -> Just sig
        Nothing -> parent env >>= lookupFunc name

insertVar :: Text -> ElaboratedType -> Env -> Env
insertVar name t env = env{variables = Map.insert name t (variables env)}

insertFunc :: Text -> ([ElaboratedType], ElaboratedType) -> Env -> Env
insertFunc name sig env = env{functions = Map.insert name sig (functions env)}

scope :: Env -> Env
scope parentEnv = Env Map.empty Map.empty (Just parentEnv)

type Check = ReaderT Env (Writer [SemanticError])

data TypedTopLevel = Expr TypedExpression | Stmt ResolvedStatement | Block SourcePos [TypedTopLevel]
    deriving (Show, Eq)

data TypedOperation = TypedOperation
    { topType :: ElaboratedType,
      topNode :: ResolvedOperation
    }
    deriving (Show, Eq)

data ResolvedOperation
    = Add TypedExpression TypedExpression
    | Subtract TypedExpression TypedExpression
    | Multiply TypedExpression TypedExpression
    | IntDivide TypedExpression TypedExpression
    | Divide TypedExpression TypedExpression
    | Modulo TypedExpression TypedExpression
    | Negation TypedExpression
    | GreaterThan TypedExpression TypedExpression
    | LessThan TypedExpression TypedExpression
    | GreaterThanEq TypedExpression TypedExpression
    | LessThanEq TypedExpression TypedExpression
    | Equals TypedExpression TypedExpression
    | NotEquals TypedExpression TypedExpression
    | Or TypedExpression TypedExpression
    | And TypedExpression TypedExpression
    | Not TypedExpression
    | BitwiseOr TypedExpression TypedExpression
    | BitwiseAnd TypedExpression TypedExpression
    | BitwiseXor TypedExpression TypedExpression
    | BitwiseNot TypedExpression
    | AddAssign TypedExpression TypedExpression
    | SubAssign TypedExpression TypedExpression
    | MulAssign TypedExpression TypedExpression
    | DivAssign TypedExpression TypedExpression
    | IntDivAssign TypedExpression TypedExpression
    | ModAssign TypedExpression TypedExpression
    | BitwiseOrAssign TypedExpression TypedExpression
    | BitwiseAndAssign TypedExpression TypedExpression
    | BitwiseXorAssign TypedExpression TypedExpression
    | Assign TypedExpression TypedExpression
    deriving (Show, Eq)

data ResolvedLiteral
    = String T.Text
    | Int Integer
    | Float Float
    | Bool Bool
    | Array [TypedExpression]
    | Vector [TypedExpression]
    | Point [TypedExpression]
    | Matrix [TypedExpression]
    deriving (Show, Eq)

data ResolvedExpression
    = LiteralExpr SourcePos ResolvedLiteral
    | Parentheses SourcePos TypedExpression
    | Identifier SourcePos Text
    | Operation SourcePos TypedOperation
    | FunctionCall SourcePos TypedExpression [TypedExpression]
    | LambdaFunc SourcePos [(TypedExpression, ElaboratedType)] TypedTopLevel
    | LambdaApplication SourcePos TypedExpression TypedExpression
    deriving (Show, Eq)

data TypedExpression = TypedExpression
    { texprType :: ElaboratedType,
      texprNode :: ResolvedExpression
    }
    deriving (Show, Eq)

data ResolvedStatement
    = Variable SourcePos TypedExpression (Maybe ElaboratedType) (Maybe TypedExpression)
    | Constant SourcePos TypedExpression ElaboratedType TypedExpression
    | Assignment SourcePos TypedOperation
    | FunctionDef
        SourcePos
        TypedExpression
        [(TypedExpression, ElaboratedType)]
        ElaboratedType
        (Maybe TypedTopLevel)
    | IfStmt SourcePos (Maybe TypedExpression) TypedTopLevel (Maybe TypedTopLevel)
    | ElseStmt SourcePos TypedTopLevel
    | Return SourcePos (Maybe TypedExpression)
    deriving (Show, Eq)
