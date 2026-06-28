module TypeCheck (module TypeCheck) where

import Data.Maybe
import Data.Map as Map hiding (map)
import Data.List
import Error
import Misc
import Types
import Prelude hiding (error)
import Context
import Debug.Trace
import qualified Data.Text as T
import FunctionStorage
import Text.Megaparsec

(<|) :: a -> Maybe a -> a
a <| b = fromMaybe a b

convertToType :: SourcePos -> ParserState -> Expression -> Type
convertToType _ _ (Literal lit) = valueToType lit
convertToType pos state (Identifier name) = getVariableType pos state name
convertToType pos state (Operation op) = checkTypeExpression pos state (Operation op)
convertToType pos state (FunctionIdentifier name) = getFunctionReturnType pos state name

inferVariableType :: SourcePos -> ParserState -> Statement -> Statement
inferVariableType _ _ (Variable exp (Just a) val) = Variable exp (Just a) val
inferVariableType _ _ (Variable exp Nothing (Just (Literal val))) = Variable exp (Just $ valueToType val) (Just (Literal val))
inferVariableType pos state decl = error pos state ("Could not infer type: " ++ show decl) "Internal error."

checkType :: SourcePos -> ParserState -> Statement -> Statement
checkType pos state (Variable exp (Just expectedType) (Just (Literal val))) =
    let actualType = convertToType pos state (Literal val)
     in if expectedType == actualType
            then Variable exp (Just expectedType) (Just (Literal val))
            else error pos state ("Expected " ++ show expectedType ++ " but got " ++ show actualType) "Consider changing the variable type or declaring a new variable."
checkType pos state (Assignment (Assign (Identifier name) (Literal val))) =
    let actualType = convertToType pos state (Literal val)
     in if convertToType pos state (Identifier name) == actualType
            then Assignment (Assign (Identifier name) (Literal val))
            else error pos state ("Expected " ++ show (convertToType pos state (Identifier name)) ++ " but got " ++ show actualType) "Consider changing the variable type or declaring a new variable."
checkType pos state (Variable exp (Just expectedType) (Just (LambdaFunc args stmt))) = Variable exp (Just expectedType) (Just (LambdaFunc args stmt))
checkType pos state (Assignment (Assign (Identifier name) (LambdaFunc args stmt))) = Assignment (Assign (Identifier name) (LambdaFunc args stmt))
checkType pos state (Variable exp (Just expectedType) (Just (FunctionCall (FunctionIdentifier name) args))) =
    let actualType = convertToType pos state (FunctionIdentifier name)
    in if expectedType == actualType
        then Variable exp (Just expectedType) (Just (FunctionCall (FunctionIdentifier name) args))
        else error pos state ("Expected " ++ show expectedType ++ " but got " ++ show actualType) "Consider changing the variable type or declaring a new variable."
checkType pos state (Assignment (Assign (Identifier varName) (FunctionCall (FunctionIdentifier name) args))) =
    let actualType = convertToType pos state (FunctionIdentifier name)
     in if convertToType pos state (Identifier varName) == actualType
            then Assignment (Assign (Identifier varName) (FunctionCall (FunctionIdentifier name) args))
            else error pos state ("Expected " ++ show (convertToType pos state (Identifier varName)) ++ " but got " ++ show actualType) "Consider changing the variable type or declaring a new variable."
checkType pos state _ = error pos state "Statement is not a variable declaration" "Internal error."

-- TODO: FUNCTIONS AND LAMBDAS
checkTypeOperation :: Expression -> Expression -> SourcePos -> ParserState -> Maybe Type -> Type
checkTypeOperation (Literal lit) (Identifier name) pos state expectedOutputType =
    if convertToType pos state (Literal lit) == convertToType pos state (Identifier name)
        then convertToType pos state (Literal lit) <| expectedOutputType
        else error pos state ("Expected " ++ show (convertToType pos state (Literal lit)) ++ " but got " ++ show (convertToType pos state (Identifier name))) "Consider changing this expression."
checkTypeOperation (Identifier name) (Literal lit) pos state expectedOutputType =
    if convertToType pos state (Literal lit) == convertToType pos state (Identifier name)
        then convertToType pos state (Literal lit) <| expectedOutputType
        else error pos state ("Expected " ++ show (convertToType pos state (Identifier name)) ++ " but got " ++ show (convertToType pos state (Literal lit))) "Consider changing this expression."
checkTypeOperation (Identifier name) (Operation op) pos state expectedOutputType =
    if convertToType pos state (Identifier name) == convertToType pos state (Operation op)
        then convertToType pos state (Identifier name) <| expectedOutputType
        else error pos state ("Expected " ++ show (convertToType pos state (Identifier name)) ++ " but got " ++ show (convertToType pos state (Operation op))) "Consider changing this expression."
checkTypeOperation (Operation op) (Identifier name) pos state expectedOutputType =
    if convertToType pos state (Identifier name) == convertToType pos state (Operation op)
        then convertToType pos state (Identifier name) <| expectedOutputType
        else error pos state ("Expected " ++ show (convertToType pos state (Operation op)) ++ " but got " ++ show (convertToType pos state (Identifier name))) "Consider changing this expression."
checkTypeOperation (Operation op) (Literal lit) pos state expectedOutputType =
    if convertToType pos state (Literal lit) == convertToType pos state (Operation op)
        then convertToType pos state (Literal lit) <| expectedOutputType
        else error pos state ("Expected " ++ show (convertToType pos state (Operation op)) ++ " but got " ++ show (convertToType pos state (Literal lit))) "Consider changing this expression."
checkTypeOperation (Literal lit) (Operation op) pos state expectedOutputType =
    if convertToType pos state (Literal lit) == convertToType pos state (Operation op)
        then convertToType pos state (Literal lit) <| expectedOutputType
        else error pos state ("Expected " ++ show (convertToType pos state (Literal lit)) ++ " but got " ++ show (convertToType pos state (Operation op))) "Consider changing this expression."
checkTypeOperation (Operation op1) (Operation op2) pos state expectedOutputType =
    if convertToType pos state (Operation op1) == convertToType pos state (Operation op2)
        then convertToType pos state (Operation op1) <| expectedOutputType
        else error pos state ("Expected " ++ show (convertToType pos state (Operation op1)) ++ " but got " ++ show (convertToType pos state (Operation op2))) "Consider changing this expression."
checkTypeOperation (Operation op) (Parentheses inner) pos state expectedOutputType =
    checkTypeOperation (Operation op) inner pos state expectedOutputType
checkTypeOperation (Literal lit) (Parentheses inner) pos state expectedOutputType =
    checkTypeOperation (Literal lit) inner pos state expectedOutputType
checkTypeOperation (Identifier name) (Parentheses inner) pos state expectedOutputType =
    checkTypeOperation (Identifier name) inner pos state expectedOutputType
checkTypeOperation (Parentheses inner) (Operation op) pos state expectedOutputType =
    checkTypeOperation (Operation op) inner pos state expectedOutputType
checkTypeOperation (Parentheses inner) (Literal lit) pos state expectedOutputType =
    checkTypeOperation (Literal lit) inner pos state expectedOutputType
checkTypeOperation (Parentheses inner) (Identifier name) pos state expectedOutputType =
    checkTypeOperation (Identifier name) inner pos state expectedOutputType

checkTypeOperationUnary :: Expression -> SourcePos -> ParserState -> Maybe Type -> Type
checkTypeOperationUnary (Literal lit) pos state expectedOutputType = convertToType pos state (Literal lit) <| expectedOutputType
checkTypeOperationUnary (Identifier name) pos state expectedOutputType = convertToType pos state (Identifier name) <| expectedOutputType
checkTypeOperationUnary (FunctionCall name args) pos state expectedOutputType = convertToType pos state name <| expectedOutputType
checkTypeOperationUnary (Operation op) pos state expectedOutputType = convertToType pos state (Operation op) <| expectedOutputType
checkTypeOperationUnary (Parentheses inner) pos state expectedOutputType = checkTypeExpression pos state inner

checkTypeExpression :: SourcePos -> ParserState -> Expression -> Type
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
checkTypeExpression pos state (Literal lit) = convertToType pos state (Literal lit)
checkTypeExpression pos state (FunctionCall name _) = convertToType pos state name 
checkTypeExpression pos state (Identifier name) = convertToType pos state (Identifier name)

checkReturnType :: Statement -> SourcePos -> ParserState -> ParserState
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

checkArguments :: Expression -> SourcePos -> ParserState -> ParserState
checkArguments (FunctionCall (FunctionIdentifier name) args) pos state =
    if expectedLength /= actualLength
        then error pos state ("Expected " ++ show expectedLength ++ " arguments but got " ++ show actualLength) "Consider adding or removing arguments, or checking the function signature."
    else 
        case mapM checkArgumentType (zip argTypes expectedArgTypes) of
            Left err -> uncurry (error pos state) err
            Right _ -> state
    where
        expectedArgTypes = getFunctionArgTypes pos state name
        expectedLength = length expectedArgTypes
        argTypes = map (checkTypeExpression pos state) args
        actualLength = length argTypes    
checkArguments _ _ state = state

checkArgumentType :: (Type, Type) -> Either (String, String) ()
checkArgumentType (actual, expected) = if actual == expected
    then Right ()
    else Left ("Expected " ++ show expected ++ " but got " ++ show actual, "Try changing the argument type.")
   
checkForReturn :: Statement -> SourcePos -> ParserState -> ParserState
checkForReturn (FunctionDef _ _ returnType (Just (Block (FunctionBlock _) stmts))) pos state =
    if not (hasReturn stmts) && returnType /= VoidT 
        then error pos state ("Function must return a value of type " ++ show returnType) "Try adding a return statement."
        else state
    
hasReturn :: [TopLevel] -> Bool
hasReturn = any isReturnStmt

isReturnStmt :: TopLevel -> Bool
isReturnStmt (Stmt (Return _)) = True
isReturnStmt _ = False

compareFunctionSignatureToForwardDecl :: Statement -> SourcePos -> ParserState -> ParserState
compareFunctionSignatureToForwardDecl (FunctionDef (FunctionIdentifier name) args returnType (Just _)) pos state
    | not (hasForwardDecl name state) = state 
    | expectedLength /= actualLength =
        error pos state ("Expected " ++ show expectedLength ++ " arguments but got " ++ show actualLength) "Consider adding or removing arguments, or checking the function signature."
    | returnType /= expectedReturnType =
        error pos state ("Expected " ++ show expectedReturnType ++ " as a return type but got " ++ show returnType) "Consider changing the return type, or checking the function signature."
    | otherwise =
        case mapM checkArgumentType (zip argTypes expectedArgTypes) of
            Left err -> uncurry (error pos state) err
            Right _ -> state
    where
        expectedArgTypes = if hasForwardDecl name state then getFunctionArgTypes pos state name else []
        expectedLength = length expectedArgTypes
        expectedReturnType = if hasForwardDecl name state then getFunctionReturnType pos state name else VoidT
        argTypes = map snd args
        actualLength = length argTypes    

