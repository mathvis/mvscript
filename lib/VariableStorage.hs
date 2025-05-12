module VariableStorage where
import Data.Text as T hiding (show)
import Types
import Data.Map as Map
import TypeCheck

initialized :: Maybe Expression -> Bool
initialized Nothing = False
initialized (Just _) = True

createVariableData :: Maybe TypeName -> Maybe Expression -> VariableData
createVariableData typename value = defaultVariableData {isInitialized=initialized value, variableType=typename}

createVariableRecord :: Declaration -> (T.Text, VariableData)
createVariableRecord (Variable (VarIdentifier name) typename val) = (,) name (createVariableData typename val)
createVariableRecord _ = error "Could not create variable record."

createArgumentVariableRecord :: (Expression, TypeName) -> (T.Text, VariableData)
createArgumentVariableRecord (VarIdentifier name, typename) = (,) name (createVariableData (Just typename) (Just (Type (Int 0))))
createArgumentVariableRecord _ = error "Could not create argument record."

addVariableToTable :: Declaration -> ParserState -> ParserState
addVariableToTable varDeclaration state = state {st=Map.insert name varData currentSt}
    where
        (name, varData) = createVariableRecord varDeclaration
        currentSt = st state

addArgumentToTable :: (Expression, TypeName) -> ParserState -> ParserState
addArgumentToTable arg state = state {st=Map.insert name argData currentSt}
    where
        (name, argData) = createArgumentVariableRecord arg
        currentSt = st state

addArgumentsToTable :: [(Expression, TypeName)] -> ParserState -> ParserState
addArgumentsToTable (arg:args) state = addArgumentsToTable args (addArgumentToTable arg state)
addArgumentsToTable [] state = state

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
    if Map.member name currentSt then
        state {st=Map.insert name varData currentSt}
    else
        error ("Variable " ++ T.unpack name ++ " is not defined.")
    where
        currentSt = st state
        varData = case Map.lookup name currentSt of
            Just vData -> vData{isInitialized=True, variableType=Just (valueToType typ)} 
            Nothing -> error ("Variable " ++ T.unpack name ++ " is not defined.")
-- TODO make it work with statements like a = b + 1;
updateVariableUninitialized _ state = state

checkScope :: Expression -> ParserState -> ParserState
checkScope (VarIdentifier name) state = case Map.lookup name (st state) of
    Just varData -> if inScope varData then
        state 
    else
        error $ "Variable " ++ show name ++ " is out of scope."
    Nothing -> error $ "Variable " ++ show name ++ " was not initialized."
checkScope _ _ = error "Could not check scope."
