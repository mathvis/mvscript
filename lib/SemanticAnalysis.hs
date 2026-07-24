module SemanticAnalysis (checkTopLevel, checkLiteral, checkExpression) where

import qualified ParserTypes as P (Expression (..), Literal (..), Statement (..), TopLevel (..))
import SemanticAnalysisTypes (Check, ElaboratedType (..), ResolvedExpression (..), ResolvedStatement, TypedExpression (..), TypedTopLevel (..), ResolvedLiteral)
import qualified SemanticAnalysisTypes as S (ResolvedLiteral (..))
import Text.Megaparsec

checkStatement :: P.Statement -> Check ResolvedStatement
checkStatement P.Variable {} = undefined
checkStatement P.Constant {} = undefined
checkStatement P.Assignment {} = undefined
checkStatement P.FunctionDef {} = undefined
checkStatement P.IfStmt {} = undefined
checkStatement P.ElseStmt {} = undefined
checkStatement P.Return {} = undefined

getTypeOfLiteral :: ResolvedLiteral -> ElaboratedType
getTypeOfLiteral (S.Array {}) = ArrayT IntT
getTypeOfLiteral (S.Vector {}) = VectorT
getTypeOfLiteral (S.Point {}) = PointT
getTypeOfLiteral (S.Matrix {}) = MatrixT
getTypeOfLiteral (S.Int {}) = IntT
getTypeOfLiteral (S.Float {}) = FloatT
getTypeOfLiteral (S.String {}) = StringT
getTypeOfLiteral (S.Bool {}) = BoolT

checkLiteral :: SourcePos -> P.Literal -> Check ResolvedLiteral
checkLiteral _ (P.Array {}) = undefined
checkLiteral _ (P.Vector {}) = undefined
checkLiteral _ (P.Point {}) = undefined
checkLiteral _ (P.Matrix {}) = undefined
checkLiteral _ (P.Int n) =  pure $ S.Int n
checkLiteral _ (P.Float n) = pure $ S.Float n
checkLiteral _ (P.String t) = pure $ S.String t
checkLiteral _ (P.Bool v) = pure $ S.Bool v

checkExpression :: P.Expression -> Check TypedExpression
checkExpression (P.Literal pos literal) =
    TypedExpression <$> typeOfLiteral <*> resolvedLiteralExpr
  where
    resolvedLiteral = checkLiteral pos literal
    resolvedLiteralExpr = (LiteralExpr pos) <$> resolvedLiteral
    typeOfLiteral = getTypeOfLiteral <$> resolvedLiteral
checkExpression P.Operation {} = undefined
checkExpression P.FunctionCall {} = undefined
checkExpression P.Parentheses {} = undefined
checkExpression P.Identifier {} = undefined
checkExpression P.LambdaFunc {} = undefined
checkExpression P.LambdaApplication {} = undefined

checkTopLevel :: P.TopLevel -> Check TypedTopLevel
checkTopLevel (P.Stmt s) = Stmt <$> checkStatement s
checkTopLevel (P.Expr e) = Expr <$> checkExpression e
checkTopLevel (P.Block pos stmts) = Block pos <$> traverse checkTopLevel stmts
