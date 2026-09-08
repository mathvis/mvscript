module SpecUtils (dummyPos, createLiteralsForErrorTestExcept, runCheck, mkEnv, ident, intLit, boolLit) where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriter)
import qualified Data.Text as T
import qualified ParserTypes as P (Expression (..), Literal (..), ParserType (..))
import SemanticAnalysisTypes (Check, Env (..), SemanticError, ElaboratedType)
import Text.Megaparsec (SourcePos)
import Text.Megaparsec.Pos (initialPos)
import qualified Data.Map as Map

dummyPos :: SourcePos
dummyPos = initialPos ""

createLiteral :: P.ParserType -> P.Literal
createLiteral P.IntT = P.Int 0
createLiteral P.FloatT = P.Float 2.5
createLiteral P.BoolT = P.Bool True
createLiteral P.StringT = P.String (T.pack "test")
createLiteral P.VectorT = P.Vector [P.Literal dummyPos (P.Int 0)]
createLiteral P.PointT = P.Point [P.Literal dummyPos (P.Int 0)]
createLiteral P.MatrixT = P.Matrix [P.Literal dummyPos (P.Array [P.Literal dummyPos (P.Int 0)])]
createLiteral (P.ArrayT P.IntT) = P.Array [P.Literal dummyPos (P.Int 0)]
createLiteral _ = error "tried creating literals for invalid types"

createLiteralsForErrorTestExcept :: [P.ParserType] -> [P.Literal]
createLiteralsForErrorTestExcept toExclude = map createLiteral types
  where
    types =
        filter
            (`notElem` toExclude)
            [P.IntT, P.FloatT, P.BoolT, P.StringT, P.VectorT, P.MatrixT, P.PointT, P.ArrayT P.IntT]

runCheck :: Env -> Check a -> (a, [SemanticError])
runCheck env m = runWriter (runReaderT m env)


mkEnv :: [(T.Text, ([ElaboratedType], ElaboratedType))] -> Env
mkEnv fns = Env Map.empty (Map.fromList fns) Nothing

ident :: T.Text -> P.Expression
ident = P.Identifier dummyPos

intLit :: Integer -> P.Expression
intLit n = P.Literal dummyPos (P.Int n)

boolLit :: Bool -> P.Expression
boolLit b = P.Literal dummyPos (P.Bool b)

