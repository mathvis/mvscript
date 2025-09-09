module VariableStorage where

import Data.Map as Map hiding (foldl)
import Data.Text as T hiding (foldl, show)
import Error
import Misc
import Text.Parsec
import TypeCheck
import Types
import Prelude hiding (fst, error)

initialized :: Maybe Expression -> Bool
initialized Nothing = False
initialized (Just _) = True

createVariableData :: Maybe TypeName -> Maybe Expression -> VariableData
createVariableData typename value = defaultVariableData{isInitialized = initialized value, variableType = typename}

createVariableRecord :: Declaration -> ParserState -> (T.Text, VariableData)
createVariableRecord (Variable (VarIdentifier name) typename val) _ = (,) name (createVariableData typename val)
createVariableRecord _ state = error defaultSourcePos state "Could not create variable record." "Internal error."

createArgumentVariableRecord :: (Expression, TypeName) -> ParserState -> (T.Text, VariableData)
createArgumentVariableRecord (VarIdentifier name, typename) state = (,) name (createVariableData (Just typename) (Just (Type (Int 0))))
createArgumentVariableRecord _ state = error defaultSourcePos state "Could not create argument record." "Internal error."

addVariableToTable :: Declaration -> ParserState -> ParserState
addVariableToTable varDeclaration state = state{st = Map.insert name varData currentSt}
  where
    (name, varData) = createVariableRecord varDeclaration state
    currentSt = st state

addArgumentToTable :: (Expression, TypeName) -> ParserState -> ParserState
addArgumentToTable arg state = state{st = Map.insert name argData currentSt}
  where
    (name, argData) = createArgumentVariableRecord arg state
    currentSt = st state

addArgumentsToTable :: [(Expression, TypeName)] -> ParserState -> ParserState
addArgumentsToTable args state = foldl (flip addArgumentToTable) state args

removeArgumentFromTable :: (Expression, TypeName) -> ParserState -> ParserState
removeArgumentFromTable (VarIdentifier name, _) state = state{st = Map.delete name (st state)}

removeArgumentsFromTable :: Declaration -> ParserState -> ParserState
removeArgumentsFromTable (FunctionDef name (arg : args) return stmts) state = removeArgumentsFromTable (FunctionDef name args return stmts) (removeArgumentFromTable arg state)
removeArgumentsFromTable (FunctionDef _ [] _ _) state = state

removeArgumentsFromTableLambda :: Expression -> ParserState -> ParserState
removeArgumentsFromTableLambda (LambdaFunc (arg : args) stmts) state = removeArgumentsFromTableLambda (LambdaFunc args stmts) (removeArgumentFromTable arg state)
removeArgumentsFromTableLambda (LambdaFunc [] _) state = state

updateVariableUninitialized :: SourcePos -> Declaration -> ParserState -> ParserState
updateVariableUninitialized pos (Assignment (Assign (VarIdentifier name) (Type typ))) state =
    case currentVData of
        Just vData -> case variableType vData of
            Nothing -> state{st = Map.insert name vData{variableType = Just (valueToType typ), isInitialized = True} currentSt}
            Just vType ->
                if vType == valueToType typ
                    then state{st = Map.insert name vData{isInitialized = True} currentSt}
                    else error pos state ("Expected " ++ show vType ++ " but got " ++ show (valueToType typ)) "Consider changing the variable type or declaring a new variable."
        Nothing -> error pos state ("Variable" ++ show name ++ " was not declared.") "Consider using the let keyword."
  where
    currentSt = st state
    currentVData = Map.lookup name currentSt
updateVariableUninitialized pos (Assignment (Assign (VarIdentifier name) (Operation op))) state =
    case currentVData of
        Just vData -> case variableType vData of
            Nothing -> state{st = Map.insert name vData{variableType = Just typ, isInitialized = True} currentSt}
            Just vType ->
                if vType == typ
                    then state{st = Map.insert name vData{isInitialized = True} currentSt}
                    else error pos state ("Expected " ++ show vType ++ " but got " ++ show typ) "Consider changing the variable type or declaring a new variable."
        Nothing -> error pos state ("Variable " ++ show name ++ " was not initialized.") "Consider using the let keyword."
  where
    currentSt = st state
    currentVData = Map.lookup name currentSt
    typ = checkTypeExpression pos state (Operation op)
updateVariableUninitialized pos (Assignment (Assign (VarIdentifier name) (VarIdentifier name2))) state =
    case currentVData of
        Just vData -> case variableType vData of
            Nothing -> state{st = Map.insert name vData{variableType = Just typ, isInitialized = True} currentSt}
            Just vType ->
                if vType == typ
                    then state{st = Map.insert name vData{isInitialized = True} currentSt}
                    else error pos state ("Expected " ++ show vType ++ " but got " ++ show typ) "Consider changing the variable type or declaring a new variable."
        Nothing -> error pos state ("Variable " ++ show name ++ " was not initialized.") "Consider using the let keyword."
  where
    currentSt = st state
    currentVData = Map.lookup name currentSt
    typ = getVariableType pos state name
updateVariableUninitialized pos (Assignment (Assign (VarIdentifier name) (Parentheses op))) state =
    updateVariableUninitialized pos (Assignment (Assign (VarIdentifier name) op)) state
updateVariableUninitialized pos (Assignment (Assign (VarIdentifier vName) (FunctionCall (FunctionIdentifier fName) args))) state =
    case currentVData of
        Just vData -> case variableType vData of
            Nothing -> state{st = Map.insert vName vData{variableType = Just typ, isInitialized = True} currentSt}
            Just vType ->
                if vType == typ
                    then state{st = Map.insert vName vData{isInitialized = True} currentSt}
                    else error pos state ("Expected " ++ show vType ++ " but got " ++ show typ) "Consider changing the variable type or declaring a new variable."
        Nothing -> error pos state ("Variable " ++ show vName ++ " was not initialized.") "Consider using the let keyword."
    where
        currentSt = st state
        currentVData = Map.lookup vName currentSt
        typ = getFunctionType pos state fName
updateVariableUninitialized _ _ state = state

checkScope :: Expression -> SourcePos -> ParserState -> ParserState
checkScope (VarIdentifier name) pos state = case Map.lookup name (st state) of
    Just varData ->
        if inScope varData
            then state
            else error pos state ("Variable " ++ show name ++ " is out of scope.") "Variable might have been initialized in a stricter scope."
    Nothing -> error pos state ("Variable " ++ show name ++ " was not initialized.") "Consider using the let keyword."
checkScope (FunctionCall (FunctionIdentifier name) _) pos state = case Map.lookup name (fst state) of
    Just funcData -> state
    Nothing -> error pos state ("Function " ++ show name ++ " was not declared.") "Consider declaring a function."
checkScope _ pos state = error pos state "Could not check scope." "Internal error."

removeScope :: VariableData -> VariableData
removeScope varData = varData{inScope = False}

removeScopeVarBlock :: Statement -> ParserState -> ParserState
removeScopeVarBlock (Decl (Variable (VarIdentifier name) _ _)) state = state{st = Map.adjust removeScope name (st state)}
removeScopeVarBlock _ state = state

removeScopeVariables :: Statement -> ParserState -> ParserState
removeScopeVariables (Block _ (stmt : stmts)) state = removeScopeVarBlock stmt state
removeScopeVariables (Block _ []) state = state

