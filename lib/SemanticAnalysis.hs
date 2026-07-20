module SemanticAnalysis (checkTopLevel) where

import qualified ParserTypes as P (Expression (..), Statement (..), TopLevel (..))
import SemanticAnalysisTypes (Check, ResolvedStatement, TypedExpression, TypedTopLevel (..))

checkStatement :: P.Statement -> Check ResolvedStatement
checkStatement P.Variable {} = undefined
checkStatement P.Constant {} = undefined
checkStatement P.Assignment {} = undefined
checkStatement P.FunctionDef {} = undefined
checkStatement P.IfStmt {} = undefined
checkStatement P.ElseStmt {} = undefined
checkStatement P.Return {} = undefined

checkExpression :: P.Expression -> Check TypedExpression
checkExpression P.Literal {} = undefined
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
