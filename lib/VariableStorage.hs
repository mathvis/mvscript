module VariableStorage where
import Data.Text as T hiding (foldl, show)
import Types
import Data.Map as Map hiding (foldl)
import TypeCheck
import Error
import Prelude hiding (error)
import Text.Parsec

initialized :: Maybe Expression -> Bool
initialized Nothing = False
initialized (Just _) = True

createVariableData :: Maybe TypeName -> Maybe Expression -> VariableData
createVariableData typename value = defaultVariableData {isInitialized=initialized value, variableType=typename}

createVariableRecord :: Declaration -> ParserState -> (T.Text, VariableData)
createVariableRecord (Variable (VarIdentifier name) typename val) state = (,) name (createVariableData typename val)
createVariableRecord _ state = error state defaultSourcePos "Could not create variable record." "Internal error."

createArgumentVariableRecord :: (Expression, TypeName) -> ParserState -> (T.Text, VariableData)
createArgumentVariableRecord (VarIdentifier name, typename) state = (,) name (createVariableData (Just typename) (Just (Type (Int 0))))
createArgumentVariableRecord _ state = error state defaultSourcePos "Could not create argument record." "Internal error."

addVariableToTable :: Declaration -> ParserState -> ParserState
addVariableToTable varDeclaration state = state {st=Map.insert name varData currentSt}
    where
        (name, varData) = createVariableRecord varDeclaration state
        currentSt = st state

addArgumentToTable :: (Expression, TypeName) -> ParserState -> ParserState
addArgumentToTable arg state = state {st=Map.insert name argData currentSt}
    where
        (name, argData) = createArgumentVariableRecord arg state
        currentSt = st state

addArgumentsToTable :: [(Expression, TypeName)] -> ParserState -> ParserState
addArgumentsToTable args state = foldl (flip addArgumentToTable) state args

removeArgumentFromTable :: (Expression, TypeName) -> ParserState -> ParserState
removeArgumentFromTable (VarIdentifier name, _) state = state {st=Map.delete name (st state)}

removeArgumentsFromTable :: Declaration -> ParserState -> ParserState
removeArgumentsFromTable (FunctionDef name (arg:args) return stmts) state = removeArgumentsFromTable (FunctionDef name args return stmts) (removeArgumentFromTable arg state)
removeArgumentsFromTable (FunctionDef _ [] _ _) state = state

removeArgumentsFromTableLambda :: Expression -> ParserState -> ParserState
removeArgumentsFromTableLambda (LambdaFunc (arg:args) stmts) state = removeArgumentsFromTableLambda (LambdaFunc args stmts) (removeArgumentFromTable arg state)
removeArgumentsFromTableLambda (LambdaFunc [] _) state = state

updateVariableUninitialized :: Declaration -> ParserState -> ParserState
updateVariableUninitialized (Assignment (Assign (VarIdentifier name) (Type typ))) state =
    state {st=Map.insert name varData currentSt}
    where
        currentSt = st state
        varData = case Map.lookup name currentSt of
            Just vData -> vData{isInitialized=True, variableType=Just (valueToType typ)} 
            Nothing -> defaultVariableData
-- TODO make it work with statements like a = b + 1;
updateVariableUninitialized _ state = state

checkScope :: SourcePos -> Expression -> ParserState -> ParserState
checkScope pos (VarIdentifier name) state = case Map.lookup name (st state) of
    Just varData -> if inScope varData then
        state 
    else
        error state pos ("Variable " ++ show name ++ " is out of scope.") "Variable might have been initialized in a stricter scope."
    Nothing -> error state pos ("Variable " ++ show name ++ " was not initialized.") "Consider using the let keyword."
checkScope pos _ state = error state pos "Could not check scope." "Internal error."

removeScope :: VariableData -> VariableData
removeScope varData = varData {inScope = False}

removeScopeVarBlock :: Statement -> ParserState -> ParserState
removeScopeVarBlock (Decl (Variable (VarIdentifier name) _ _)) state = state {st=Map.adjust removeScope name (st state)}
removeScopeVarBlock _ state = state

removeScopeVariables :: Statement -> ParserState -> ParserState
removeScopeVariables (Block _ (stmt:stmts)) state = removeScopeVarBlock stmt state
removeScopeVariables (Block _ []) state = state
