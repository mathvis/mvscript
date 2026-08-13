module SemanticAnalysis (checkTopLevel, checkLiteral, checkExpression, unifyTypesMVContainers) where

import Control.Monad
import Control.Monad.Writer (tell)
import qualified ParserTypes as P (Expression (..), Literal (..), Statement (..), TopLevel (..))
import SemanticAnalysisTypes (Check, ElaboratedType (..), ResolvedExpression (..), ResolvedLiteral, ResolvedStatement, SemanticError (..), TypedExpression (..), TypedTopLevel (..))
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
getTypeOfLiteral (S.Array []) = ArrayT UnknownT
getTypeOfLiteral (S.Array (expr:_)) = ArrayT (texprType expr)
getTypeOfLiteral (S.Vector {}) = VectorT
getTypeOfLiteral (S.Point {}) = PointT
getTypeOfLiteral (S.Matrix {}) = MatrixT
getTypeOfLiteral (S.Int {}) = IntT
getTypeOfLiteral (S.Float {}) = FloatT
getTypeOfLiteral (S.String {}) = StringT
getTypeOfLiteral (S.Bool {}) = BoolT

unifyTypesMVContainers :: [ElaboratedType] -> Either SemanticError ElaboratedType
unifyTypesMVContainers [] = Left EmptyMVContainer
unifyTypesMVContainers (t : ts)
  | not (isNumeric t) = Left (TypeMismatch [IntT, FloatT] t)
  | otherwise = foldM combine t ts
  where
    isNumeric IntT = True
    isNumeric FloatT = True
    isNumeric _ = False
    combine a b
      | not $ isNumeric a = Left (TypeMismatch [IntT, FloatT] a)
      | not $ isNumeric b = Left (TypeMismatch [IntT, FloatT] b)
      | a == b = Right a
      | otherwise = Left (TypeMismatch [a] b)

unifyTypesMatrix :: [ElaboratedType] -> Either SemanticError ElaboratedType
unifyTypesMatrix [] = Left EmptyMVContainer
unifyTypesMatrix (t : ts)
  | not (isArray t) = Left (TypeMismatch [(ArrayT IntT), (ArrayT FloatT)] t)
  | otherwise = foldM combine t ts
  where
    isArray (ArrayT IntT) = True
    isArray (ArrayT FloatT) = True
    isArray _ = False
    combine a b
      | not $ isArray a = Left (TypeMismatch [(ArrayT IntT), (ArrayT FloatT)] a)
      | not $ isArray b = Left (TypeMismatch [(ArrayT IntT), (ArrayT FloatT)] b)
      | a == b = Right a
      | otherwise = Left (TypeMismatch [a] b)

unifyArrayTypes :: [ElaboratedType] -> Either SemanticError ElaboratedType
unifyArrayTypes [] = Right UnknownT
unifyArrayTypes (t:ts) = foldM combine t ts
  where
    combine a b
      | a == b = Right a
      | otherwise = Left (TypeMismatch [a] b)


checkLiteral :: SourcePos -> P.Literal -> Check ResolvedLiteral
checkLiteral _ (P.Array exprs) = do
  typedElems <- traverse checkExpression exprs
  let elemTypes = map texprType typedElems
  _ <- case unifyArrayTypes elemTypes of
    Right ty -> pure ty
    Left err -> do
      tell [err]
      return ErrorT
  return $ S.Array typedElems
checkLiteral _ (P.Vector exprs) = do
  typedElems <- traverse checkExpression exprs
  let elemTypes = map texprType typedElems
  _ <- case unifyTypesMVContainers elemTypes of
    Right ty -> pure ty
    Left err -> do
      tell [err]
      return ErrorT
  return $ S.Vector typedElems
checkLiteral _ (P.Point exprs) = do
  typedElems <- traverse checkExpression exprs
  let elemTypes = map texprType typedElems
  _ <- case unifyTypesMVContainers elemTypes of
    Right ty -> pure ty
    Left err -> do
      tell [err]
      return ErrorT
  return $ S.Point typedElems
checkLiteral _ (P.Matrix exprs) = do
  typedElems <- traverse checkExpression exprs
  let elemTypes = map texprType typedElems
  _ <- case unifyTypesMatrix elemTypes of
    Right ty -> pure ty
    Left err -> do
      tell [err]
      return ErrorT
  return $ S.Matrix typedElems
checkLiteral _ (P.Int n) = pure $ S.Int n
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
