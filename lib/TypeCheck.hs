module TypeCheck where

import Error
import Text.Parsec
import Types
import Prelude hiding (error)
import VariableStorage 
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

