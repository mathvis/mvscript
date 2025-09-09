module TypeCheck where

import Data.Maybe
import Error
import Misc
import Text.Parsec
import Types
import Prelude hiding (error)
import Context
import Debug.Trace

(<|) :: a -> Maybe a -> a
a <| b = fromMaybe a b

convertToTypeName :: SourcePos -> ParserState -> Expression -> TypeName
convertToTypeName _ _ (Type typ) = valueToType typ
convertToTypeName pos state (VarIdentifier name) = getVariableType pos state name
convertToTypeName pos state (Operation op) = checkTypeExpression pos state (Operation op)
convertToTypeName pos state (FunctionIdentifier name) = getFunctionType pos state name

inferVariableType :: SourcePos -> ParserState -> Declaration -> Declaration
inferVariableType _ _ (Variable exp (Just a) val) = Variable exp (Just a) val
inferVariableType _ _ (Variable exp Nothing (Just (Type val))) = Variable exp (Just $ valueToType val) (Just (Type val))
inferVariableType pos state decl = error pos state ("Could not infer type: " ++ show decl) "Internal error."

checkType :: SourcePos -> ParserState -> Declaration -> Declaration
checkType pos state (Variable exp (Just expectedType) (Just (Type val))) =
    let actualType = convertToTypeName pos state (Type val)
     in if expectedType == actualType
            then Variable exp (Just expectedType) (Just (Type val))
            else error pos state ("Expected " ++ show expectedType ++ " but got " ++ show actualType) "Consider changing the variable type or declaring a new variable."
checkType pos state (Assignment (Assign (VarIdentifier name) (Type val))) =
    let actualType = convertToTypeName pos state (Type val)
     in if convertToTypeName pos state (VarIdentifier name) == actualType
            then Assignment (Assign (VarIdentifier name) (Type val))
            else error pos state ("Expected " ++ show (convertToTypeName pos state (VarIdentifier name)) ++ " but got " ++ show actualType) "Consider changing the variable type or declaring a new variable."
checkType pos state (Variable exp (Just expectedType) (Just (LambdaFunc args stmt))) = Variable exp (Just expectedType) (Just (LambdaFunc args stmt))
checkType pos state (Assignment (Assign (VarIdentifier name) (LambdaFunc args stmt))) = Assignment (Assign (VarIdentifier name) (LambdaFunc args stmt))
checkType pos state (Variable exp (Just expectedType) (Just (FunctionCall (FunctionIdentifier name) args))) =
    let actualType = convertToTypeName pos state (FunctionIdentifier name)
    in if expectedType == actualType
        then Variable exp (Just expectedType) (Just (FunctionCall (FunctionIdentifier name) args))
        else error pos state ("Expected " ++ show expectedType ++ " but got " ++ show actualType) "Consider changing the variable type or declaring a new variable."
checkType pos state (Assignment (Assign (VarIdentifier varName) (FunctionCall (FunctionIdentifier name) args))) =
    let actualType = convertToTypeName pos state (FunctionIdentifier name)
     in if convertToTypeName pos state (VarIdentifier varName) == actualType
            then Assignment (Assign (VarIdentifier varName) (FunctionCall (FunctionIdentifier name) args))
            else error pos state ("Expected " ++ show (convertToTypeName pos state (VarIdentifier varName)) ++ " but got " ++ show actualType) "Consider changing the variable type or declaring a new variable."
checkType pos state _ = error pos state "Declaration is not a variable declaration" "Internal error."

-- TODO: FUNCTIONS AND LAMBDAS
checkTypeOperation :: Expression -> Expression -> SourcePos -> ParserState -> Maybe TypeName -> TypeName
checkTypeOperation (Type typ) (VarIdentifier name) pos state expectedOutputType =
    if convertToTypeName pos state (Type typ) == convertToTypeName pos state (VarIdentifier name)
        then convertToTypeName pos state (Type typ) <| expectedOutputType
        else error pos state ("Expected " ++ show (convertToTypeName pos state (Type typ)) ++ " but got " ++ show (convertToTypeName pos state (VarIdentifier name))) "Consider changing this expression."
checkTypeOperation (VarIdentifier name) (Type typ) pos state expectedOutputType =
    if convertToTypeName pos state (Type typ) == convertToTypeName pos state (VarIdentifier name)
        then convertToTypeName pos state (Type typ) <| expectedOutputType
        else error pos state ("Expected " ++ show (convertToTypeName pos state (VarIdentifier name)) ++ " but got " ++ show (convertToTypeName pos state (Type typ))) "Consider changing this expression."
checkTypeOperation (VarIdentifier name) (Operation op) pos state expectedOutputType =
    if convertToTypeName pos state (VarIdentifier name) == convertToTypeName pos state (Operation op)
        then convertToTypeName pos state (VarIdentifier name) <| expectedOutputType
        else error pos state ("Expected " ++ show (convertToTypeName pos state (VarIdentifier name)) ++ " but got " ++ show (convertToTypeName pos state (Operation op))) "Consider changing this expression."
checkTypeOperation (Operation op) (VarIdentifier name) pos state expectedOutputType =
    if convertToTypeName pos state (VarIdentifier name) == convertToTypeName pos state (Operation op)
        then convertToTypeName pos state (VarIdentifier name) <| expectedOutputType
        else error pos state ("Expected " ++ show (convertToTypeName pos state (Operation op)) ++ " but got " ++ show (convertToTypeName pos state (VarIdentifier name))) "Consider changing this expression."
checkTypeOperation (Operation op) (Type typ) pos state expectedOutputType =
    if convertToTypeName pos state (Type typ) == convertToTypeName pos state (Operation op)
        then convertToTypeName pos state (Type typ) <| expectedOutputType
        else error pos state ("Expected " ++ show (convertToTypeName pos state (Operation op)) ++ " but got " ++ show (convertToTypeName pos state (Type typ))) "Consider changing this expression."
checkTypeOperation (Type typ) (Operation op) pos state expectedOutputType =
    if convertToTypeName pos state (Type typ) == convertToTypeName pos state (Operation op)
        then convertToTypeName pos state (Type typ) <| expectedOutputType
        else error pos state ("Expected " ++ show (convertToTypeName pos state (Type typ)) ++ " but got " ++ show (convertToTypeName pos state (Operation op))) "Consider changing this expression."
checkTypeOperation (Operation op1) (Operation op2) pos state expectedOutputType =
    if convertToTypeName pos state (Operation op1) == convertToTypeName pos state (Operation op2)
        then convertToTypeName pos state (Operation op1) <| expectedOutputType
        else error pos state ("Expected " ++ show (convertToTypeName pos state (Operation op1)) ++ " but got " ++ show (convertToTypeName pos state (Operation op2))) "Consider changing this expression."
checkTypeOperation (Operation op) (Parentheses inner) pos state expectedOutputType =
    checkTypeOperation (Operation op) inner pos state expectedOutputType
checkTypeOperation (Type typ) (Parentheses inner) pos state expectedOutputType =
    checkTypeOperation (Type typ) inner pos state expectedOutputType
checkTypeOperation (VarIdentifier name) (Parentheses inner) pos state expectedOutputType =
    checkTypeOperation (VarIdentifier name) inner pos state expectedOutputType
checkTypeOperation (Parentheses inner) (Operation op) pos state expectedOutputType =
    checkTypeOperation (Operation op) inner pos state expectedOutputType
checkTypeOperation (Parentheses inner) (Type typ) pos state expectedOutputType =
    checkTypeOperation (Type typ) inner pos state expectedOutputType
checkTypeOperation (Parentheses inner) (VarIdentifier name) pos state expectedOutputType =
    checkTypeOperation (VarIdentifier name) inner pos state expectedOutputType

checkTypeOperationUnary :: Expression -> SourcePos -> ParserState -> Maybe TypeName -> TypeName
checkTypeOperationUnary (Type typ) pos state expectedOutputType = convertToTypeName pos state (Type typ) <| expectedOutputType
checkTypeOperationUnary (VarIdentifier name) pos state expectedOutputType = convertToTypeName pos state (VarIdentifier name) <| expectedOutputType
checkTypeOperationUnary (Operation op) pos state expectedOutputType = convertToTypeName pos state (Operation op) <| expectedOutputType
checkTypeOperationUnary (Parentheses inner) pos state expectedOutputType = checkTypeExpression pos state inner

checkTypeExpression :: SourcePos -> ParserState -> Expression -> TypeName
checkTypeExpression pos state (Operation op) = case op of
    Add x y -> checkTypeOperation x y pos state Nothing
    Subtract x y -> checkTypeOperation x y pos state Nothing
    Multiply x y -> checkTypeOperation x y pos state Nothing
    IntDivide x y -> checkTypeOperation x y pos state (Just IntT)
    Divide x y -> checkTypeOperation x y pos state (Just FloatT)
    Modulo x y -> checkTypeOperation x y pos state (Just IntT)
    Negation x -> checkTypeOperationUnary x pos state Nothing
    GreaterThan x y -> checkTypeOperation x y pos state (Just BoolT)
    LessThan x y -> checkTypeOperation x y pos state (Just BoolT)
    GreaterThanEq x y -> checkTypeOperation x y pos state (Just BoolT)
    LessThanEq x y -> checkTypeOperation x y pos state (Just BoolT)
    Equals x y -> checkTypeOperation x y pos state (Just BoolT)
    NotEquals x y -> checkTypeOperation x y pos state (Just BoolT)
    Or x y -> checkTypeOperation x y pos state (Just BoolT)
    And x y -> checkTypeOperation x y pos state (Just BoolT)
    Not x -> checkTypeOperationUnary x pos state (Just BoolT)
    BitwiseOr x y -> checkTypeOperation x y pos state (Just IntT)
    BitwiseAnd x y -> checkTypeOperation x y pos state (Just IntT)
    BitwiseXor x y -> checkTypeOperation x y pos state (Just IntT)
    BitwiseNot x -> checkTypeOperationUnary x pos state (Just IntT)
    _ -> error pos state "Not a valid operation" "Internal error."
checkTypeExpression pos state (Parentheses op) = checkTypeExpression pos state op
checkTypeExpression pos state (Type typ) = convertToTypeName pos state (Type typ)

checkReturnType :: Expression -> SourcePos -> ParserState -> ParserState
checkReturnType (Return expr) pos state =
    let
        returnType = getCurrentFunctionReturnType pos state
        expressionType = case expr of
            Just exp -> checkTypeExpression pos state exp
            Nothing -> VoidT
    in if returnType == expressionType then
        state
    else
        error pos state ("Expected " ++ show returnType ++ " but got " ++ show expressionType) "Consider changing the return type or declaring a new function."
