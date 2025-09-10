module Context (module Context) where
import Types
import Text.Parsec
import Error
import Prelude hiding (error)

changeContext :: BlockType -> ParserState -> ParserState
changeContext blocktype state = state{context = Just blocktype} 

resetContext :: ParserState -> ParserState
resetContext state = state{context=Nothing}

checkForBlock :: SourcePos -> ParserState -> ParserState
checkForBlock pos state = case context state of
    Just (FunctionBlock _) -> state
    Just NoType -> state
    _ -> error pos state "A return statement must be inside a block." "Try putting this statement inside of a block or function."

getCurrentFunctionReturnType :: SourcePos -> ParserState -> TypeName
getCurrentFunctionReturnType pos state = case context state of
    Nothing -> error pos state "A return statement must be inside a block." "Try putting this statement inside of a block or function."
    Just (FunctionBlock typename) -> typename
    Just NoType -> VoidT -- TODO: typecheck normal blocks when assigning variables
    _ -> error pos state "A return statement must be inside a block." "Try putting this statement inside of a block or function."

    
