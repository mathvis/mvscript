module Context (module Context) where
import Types
import Text.Parsec
import Error
import Prelude hiding (error)
import Data.List
import Data.Maybe

isFunction :: BlockType -> Bool
isFunction (FunctionBlock _) = True
isFunction _ = False 

findFunction :: [BlockType] -> Maybe BlockType
findFunction = find isFunction

changeContext :: BlockType -> ParserState -> ParserState
changeContext blocktype state = state{context = newContext} 
    where
        previousContext = context state
        newContext = blocktype:previousContext

removeContext :: ParserState -> ParserState
removeContext state = state{context = newContext}
    where
        previousContext = context state
        newContext = tail previousContext

checkForBlock :: SourcePos -> ParserState -> ParserState
checkForBlock pos state = case listToMaybe (context state) of
    Just (FunctionBlock _) -> state
    _ -> error pos state "A return statement must be inside a function." "Try putting this statement inside of a function."

getCurrentFunctionReturnType :: SourcePos -> ParserState -> TypeName
getCurrentFunctionReturnType pos state =
    case findFunction ctx of
        Nothing -> error pos state "A return statement must be inside a function." "Try putting this statement inside of a function."
        Just (FunctionBlock typename) -> typename
    where
        ctx = context state

checkForSecondOrderFunction :: SourcePos -> ParserState -> ParserState
checkForSecondOrderFunction pos state =
    if any isFunction ctx
        then error pos state "A function cannot be inside another function." "Try going outside this function."
        else state
    where
        ctx = context state

    
