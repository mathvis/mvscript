module SemanticAnalysis (checkTopLevel, checkLiteral, checkExpression, checkOperation, checkFunctionCall) where

import Control.Monad
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.Writer (tell)
import qualified ParserTypes as P (
    Expression (..),
    Literal (..),
    Operation (..),
    Statement (..),
    TopLevel (..),
 )
import SemanticAnalysisTypes (
    Check,
    ElaboratedType (..),
    ResolvedExpression (..),
    ResolvedLiteral,
    ResolvedOperation (..),
    ResolvedStatement,
    SemanticError (..),
    TypedExpression (..),
    TypedOperation (..),
    TypedTopLevel (..),
    fromLiteral,
    lookupFunc,
 )
import qualified SemanticAnalysisTypes as S (ResolvedLiteral (..))
import Text.Megaparsec

checkStatement :: P.Statement -> Check ResolvedStatement
checkStatement P.Variable{} = undefined
checkStatement P.Constant{} = undefined
checkStatement P.Assignment{} = undefined
checkStatement P.FunctionDef{} = undefined
checkStatement P.IfStmt{} = undefined
checkStatement P.ElseStmt{} = undefined
checkStatement P.Return{} = undefined

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
    | not (isArray t) = Left (TypeMismatch [ArrayT IntT, ArrayT FloatT] t)
    | otherwise = foldM combine t ts
  where
    isArray (ArrayT IntT) = True
    isArray (ArrayT FloatT) = True
    isArray _ = False
    combine a b
        | not $ isArray a = Left (TypeMismatch [ArrayT IntT, ArrayT FloatT] a)
        | not $ isArray b = Left (TypeMismatch [ArrayT IntT, ArrayT FloatT] b)
        | a == b = Right a
        | otherwise = Left (TypeMismatch [a] b)

unifyArrayTypes :: [ElaboratedType] -> Either SemanticError ElaboratedType
unifyArrayTypes [] = Right UnknownT
unifyArrayTypes (t : ts) = foldM combine t ts
  where
    combine a b
        | a == b = Right a
        | otherwise = Left (TypeMismatch [a] b)

checkOperation :: SourcePos -> P.Operation -> Check TypedOperation
checkOperation pos op
    | isUnary op = checkOperationUnary pos op
    | isBinaryLogic op = checkOperationBinaryLogic pos op
    -- \| isAssignment op = checkOperationAssignment pos op
    | isComparison op = checkOperationComparison pos op
    | otherwise = checkOperationBinaryArithmetic pos op
  where
    isUnary P.Negation{} = True
    isUnary P.Not{} = True
    isUnary _ = False
    isBinaryLogic P.Or{} = True
    isBinaryLogic P.And{} = True
    isBinaryLogic P.BitwiseAnd{} = True
    isBinaryLogic P.BitwiseOr{} = True
    isBinaryLogic P.BitwiseXor{} = True
    isBinaryLogic _ = False
    -- isAssignment P.Assign{} = True
    -- isAssignment P.AddAssign{} = True
    -- isAssignment P.SubAssign{} = True
    -- isAssignment P.MulAssign{} = True
    -- isAssignment P.DivAssign{} = True
    -- isAssignment P.IntDivAssign{} = True
    -- isAssignment P.ModAssign{} = True
    -- isAssignment P.BitwiseAndAssign{} = True
    -- isAssignment P.BitwiseOrAssign{} = True
    -- isAssignment P.BitwiseXorAssign{} = True
    -- isAssignment _ = False
    isComparison P.Equals{} = True
    isComparison P.NotEquals{} = True
    isComparison P.GreaterThan{} = True
    isComparison P.GreaterThanEq{} = True
    isComparison P.LessThan{} = True
    isComparison P.LessThanEq{} = True
    isComparison _ = False

checkOperationBinaryArithmetic :: SourcePos -> P.Operation -> Check TypedOperation
checkOperationBinaryArithmetic _ op = do
    let (x, y) = case op of
            P.Add x' y' -> (x', y')
            P.Subtract x' y' -> (x', y')
            P.Multiply x' y' -> (x', y')
            P.Divide x' y' -> (x', y')
            P.IntDivide x' y' -> (x', y')
            P.Modulo x' y' -> (x', y')
            _ -> error "not a binary arithmetic operator"
    typedX <- checkExpression x
    typedY <- checkExpression y
    let xType = texprType typedX
    let yType = texprType typedY
    resultType <-
        if xType `elem` validTypes op
            then
                if xType == yType
                    then pure (resultTypeFor op xType)
                    else tell [TypeMismatch [xType] yType] >> pure ErrorT
            else
                tell [TypeMismatch (validTypes op) xType] >> pure ErrorT
    pure $ mkTypedOp op resultType typedX typedY
  where
    validTypes P.Add{} = [IntT, FloatT, StringT]
    validTypes P.Subtract{} = [IntT, FloatT]
    validTypes P.Multiply{} = [IntT, FloatT]
    validTypes P.Divide{} = [IntT, FloatT]
    validTypes P.IntDivide{} = [IntT, FloatT]
    validTypes P.Modulo{} = [IntT, FloatT]
    validTypes _ = error "not a binary arithmetic operator"

    resultTypeFor P.Divide{} _ = FloatT
    resultTypeFor _ t = t

checkOperationComparison :: SourcePos -> P.Operation -> Check TypedOperation
checkOperationComparison _ (P.Equals x y) = do
    typedX <- checkExpression x
    typedY <- checkExpression y
    let xType = texprType typedX
    let yType = texprType typedY
    resultType <-
        if xType == yType
            then
                pure BoolT
            else
                tell [TypeMismatch [xType] yType] >> pure ErrorT
    pure $ mkTypedOp (P.Equals x y) resultType typedX typedY
checkOperationComparison _ (P.NotEquals x y) = do
    typedX <- checkExpression x
    typedY <- checkExpression y
    let xType = texprType typedX
    let yType = texprType typedY
    resultType <-
        if xType == yType
            then
                pure BoolT
            else
                tell [TypeMismatch [xType] yType] >> pure ErrorT
    pure $ mkTypedOp (P.NotEquals x y) resultType typedX typedY
checkOperationComparison _ op = do
    let (x, y) = case op of
            P.GreaterThan x' y' -> (x', y')
            P.GreaterThanEq x' y' -> (x', y')
            P.LessThan x' y' -> (x', y')
            P.LessThanEq x' y' -> (x', y')
            _ -> error "not a binary comparison operator"
    typedX <- checkExpression x
    typedY <- checkExpression y
    let xType = texprType typedX
    let yType = texprType typedY
    resultType <-
        if isNumber xType
            then
                if isNumber yType && yType == xType
                    then
                        pure BoolT
                    else
                        tell [TypeMismatch [xType] yType] >> pure ErrorT
            else
                tell [TypeMismatch [IntT, FloatT] xType] >> pure ErrorT
    pure $ mkTypedOp op resultType typedX typedY
  where
    isNumber IntT = True
    isNumber FloatT = True
    isNumber _ = False

mkTypedOpUnary :: P.Operation -> ElaboratedType -> TypedExpression -> TypedOperation
mkTypedOpUnary P.Negation{} t x = TypedOperation t (Negation x)
mkTypedOpUnary P.Not{} t x = TypedOperation t (Not x)
mkTypedOpUnary _ _ _ = error "binary operation found in unary context"

mkTypedOp :: P.Operation -> ElaboratedType -> TypedExpression -> TypedExpression -> TypedOperation
mkTypedOp P.Or{} t x y = TypedOperation t (Or x y)
mkTypedOp P.And{} t x y = TypedOperation t (And x y)
mkTypedOp P.BitwiseAnd{} t x y = TypedOperation t (BitwiseAnd x y)
mkTypedOp P.BitwiseXor{} t x y = TypedOperation t (BitwiseXor x y)
mkTypedOp P.BitwiseOr{} t x y = TypedOperation t (BitwiseOr x y)
mkTypedOp P.GreaterThan{} t x y = TypedOperation t (GreaterThan x y)
mkTypedOp P.GreaterThanEq{} t x y = TypedOperation t (GreaterThanEq x y)
mkTypedOp P.LessThan{} t x y = TypedOperation t (LessThan x y)
mkTypedOp P.LessThanEq{} t x y = TypedOperation t (LessThanEq x y)
mkTypedOp P.Equals{} t x y = TypedOperation t (Equals x y)
mkTypedOp P.NotEquals{} t x y = TypedOperation t (NotEquals x y)
mkTypedOp P.Add{} t x y = TypedOperation t (Add x y)
mkTypedOp P.Subtract{} t x y = TypedOperation t (Subtract x y)
mkTypedOp P.Multiply{} t x y = TypedOperation t (Multiply x y)
mkTypedOp P.Divide{} t x y = TypedOperation t (Divide x y)
mkTypedOp P.IntDivide{} t x y = TypedOperation t (IntDivide x y)
mkTypedOp P.Modulo{} t x y = TypedOperation t (Modulo x y)
mkTypedOp _ _ _ _ = undefined

checkOperationBinaryLogic :: SourcePos -> P.Operation -> Check TypedOperation
checkOperationBinaryLogic _ op = do
    let (x, y, expected) = case op of
            P.Or x' y' -> (x', y', BoolT)
            P.And x' y' -> (x', y', BoolT)
            P.BitwiseOr x' y' -> (x', y', IntT)
            P.BitwiseAnd x' y' -> (x', y', IntT)
            P.BitwiseXor x' y' -> (x', y', IntT)
            _ -> error "not a binary logic operator"
    typedX <- checkExpression x
    typedY <- checkExpression y
    let xType = texprType typedX
    let yType = texprType typedY
    resultType <- case xType of
        t | t == expected -> case yType of
            t' | t' == expected -> pure expected
            _ -> tell [TypeMismatch [expected] yType] >> pure ErrorT
        _ -> tell [TypeMismatch [expected] xType] >> pure ErrorT
    pure $ mkTypedOp op resultType typedX typedY

-- checkOperationBinary :: SourcePos -> P.Operation -> CheckTypedOperation

checkOperationUnary :: SourcePos -> P.Operation -> Check TypedOperation
checkOperationUnary _ (P.Negation x) = do
    typedX <- checkExpression x
    let xType = texprType typedX
    resultType <-
        if isNumber xType
            then
                pure xType
            else do
                tell [TypeMismatch [IntT, FloatT] xType]
                pure ErrorT
    pure $ mkTypedOpUnary (P.Negation x) resultType typedX
  where
    isNumber IntT = True
    isNumber FloatT = True
    isNumber _ = False
checkOperationUnary _ (P.Not x) = do
    typedX <- checkExpression x
    let xType = texprType typedX
    resultType <- case xType of
        BoolT -> pure BoolT
        _ -> do
            tell [TypeMismatch [BoolT] xType]
            pure ErrorT
    pure $ mkTypedOpUnary (P.Not x) resultType typedX
checkOperationUnary _ _ = error "binary operation found in unary context"

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
    resolvedLiteralExpr = LiteralExpr pos <$> resolvedLiteral
    typeOfLiteral = fromLiteral <$> resolvedLiteral
checkExpression (P.Operation pos operation) =
    TypedExpression <$> exprType <*> resolvedExpr
  where
    resolvedOperation = checkOperation pos operation
    resolvedExpr = Operation pos <$> resolvedOperation
    exprType = topType <$> resolvedOperation
checkExpression (P.FunctionCall pos name args) = do
    (resolvedName, resolvedArgs) <- checkFunctionCall name args
    let resolvedExpr = FunctionCall pos resolvedName resolvedArgs
        exprType = texprType resolvedName
    return (TypedExpression exprType resolvedExpr)
checkExpression (P.Parentheses pos expr) =
    TypedExpression <$> exprType <*> resolvedExpr
  where
    resolvedInnerExpr = checkExpression expr
    resolvedExpr = Parentheses pos <$> resolvedInnerExpr
    exprType = texprType <$> resolvedInnerExpr
checkExpression P.Identifier{} = undefined
checkExpression P.LambdaFunc{} = undefined
checkExpression P.LambdaApplication{} = undefined

checkFunctionCall :: P.Expression -> [P.Expression] -> Check (TypedExpression, [TypedExpression])
checkFunctionCall name args = do
    env <- ask
    resolvedArgs <- traverse checkExpression args
    resolvedNameExpr <- checkExpression name
    let resolvedName = case resolvedNameExpr of
            TypedExpression _ (Identifier _ n) -> n
            _ -> error "found non-identifier in function call identifier position"
    _ <- case lookupFunc resolvedName env of
        Nothing ->
            tell [UseOfUndeclaredIdentifier resolvedName] >> pure ErrorT
        Just (argTypes, returnType) -> do
            if argTypes /= map texprType resolvedArgs
                then
                    tell [InvalidArguments resolvedName argTypes (map texprType resolvedArgs)] >> pure ErrorT
                else
                    pure returnType
    return (resolvedNameExpr, resolvedArgs)

checkTopLevel :: P.TopLevel -> Check TypedTopLevel
checkTopLevel (P.Stmt s) = Stmt <$> checkStatement s
checkTopLevel (P.Expr e) = Expr <$> checkExpression e
checkTopLevel (P.Block pos stmts) = Block pos <$> traverse checkTopLevel stmts

