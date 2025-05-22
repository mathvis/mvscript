module TypeCheck where

import Error
import Text.Parsec
import Types
import Prelude hiding (error)
import Misc


inferVariableType :: SourcePos -> ParserState -> Declaration -> Declaration
inferVariableType _ _ (Variable exp (Just a) val) = Variable exp (Just a) val
inferVariableType _ _ (Variable exp Nothing (Just (Type val))) = Variable exp (Just $ valueToType val) (Just (Type val))
inferVariableType pos state decl = error state pos ("Could not infer type: " ++ show decl) "Internal error."

checkType :: SourcePos -> ParserState -> Declaration -> Declaration
checkType pos state (Variable exp (Just expectedType) (Just (Type val))) =
    let actualType = valueToType val
     in if expectedType == actualType
            then Variable exp (Just expectedType) (Just (Type val))
            else error state pos ("Expected " ++ show expectedType ++ " but got " ++ show actualType) "Consider changing the variable type or declaring a new variable."
checkType pos state (Assignment (Assign (VarIdentifier name) (Type val))) =
    let actualType = valueToType val
     in if getVariableType pos name state == actualType
            then Assignment (Assign (VarIdentifier name) (Type val))
            else error state pos ("Expected " ++ show (getVariableType pos name state) ++ " but got " ++ show actualType) "Consider changing the variable type or declaring a new variable."
checkType pos state _ = error state pos "Declaration is not a variable declaration" "Internal error."

-- TODO: needs to allow:
--    - type + variable ✓
--    - variable + type ✓
--    - variable + operation
--    - operation + variable
--    - type + operation
--    - operation + type
checkTypeOperation :: Expression -> Expression -> SourcePos -> ParserState -> TypeName
checkTypeOperation (Type typ) (VarIdentifier name) pos state = if valueToType typ == getVariableType pos name state
    then valueToType typ
    else error state pos ("Expected " ++ show (valueToType typ) ++ " but got " ++ show (getVariableType pos name state)) "Consider changing this expression."
checkTypeOperation  (VarIdentifier name) (Type typ) pos state = if valueToType typ == getVariableType pos name state
    then valueToType typ
    else error state pos ("Expected " ++ show (getVariableType pos name state) ++ " but got " ++ show (valueToType typ)) "Consider changing this expression."

checkTypeExpression :: SourcePos -> ParserState -> Expression -> TypeName
checkTypeExpression pos state (Operation op) = case op of
    Add x y -> checkTypeOperation x y pos state
    Subtract x y -> checkTypeOperation x y pos state
    Multiply x y -> checkTypeOperation x y pos state
    IntDivide x y -> checkTypeOperation x y pos state
    Divide x y -> checkTypeOperation x y pos state
    Modulo x y -> checkTypeOperation x y pos state
    -- Negation x -> checkTypeOperationUnary x pos state
    GreaterThan x y -> checkTypeOperation x y pos state
    LessThan x y -> checkTypeOperation x y pos state
    GreaterThanEq x y -> checkTypeOperation x y pos state
    LessThanEq x y -> checkTypeOperation x y pos state
    Equals x y -> checkTypeOperation x y pos state
    NotEquals x y -> checkTypeOperation x y pos state
    Or x y -> checkTypeOperation x y pos state
    And x y -> checkTypeOperation x y pos state
    -- Not x -> checkTypeOperationUnary x pos state
    BitwiseOr x y -> checkTypeOperation x y pos state
    BitwiseAnd x y -> checkTypeOperation x y pos state
    BitwiseXor x y -> checkTypeOperation x y pos state
    -- BitwiseNot x -> checkTypeOperationUnary x pos state
    _ -> error state pos "Not a valid operation" "Internal error."

