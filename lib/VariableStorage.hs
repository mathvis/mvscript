module VariableStorage where
import Data.Text as T
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


addVariableToTable :: Declaration -> ParserState -> ParserState
addVariableToTable varDeclaration state = state {st=Map.insert name varData currentSt}
    where
        (name, varData) = createVariableRecord varDeclaration
        currentSt = st state

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
