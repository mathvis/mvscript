{-# LANGUAGE NamedFieldPuns #-}

module FunctionStorage (module FunctionStorage) where

import Data.Map as Map hiding (foldl, map)
import Data.List as List
import qualified Data.Text as T
import Error
import Types
import Prelude hiding (error, fst)
import Text.Parsec

argIdsToText :: [(Expression, TypeName)] -> [(T.Text, TypeName)]
argIdsToText = map argIdToText
    where
        argIdToText (VarIdentifier name, typename) = (name, typename)

createFunctionData :: [(Expression, TypeName)] -> TypeName -> Bool -> FunctionData
createFunctionData args returnType hasBody =
    defaultFunctionData {returnType, arguments = argIdsToText args, hasBody}

createFunctionRecord :: Declaration -> Bool -> ParserState -> (T.Text, FunctionData)
createFunctionRecord (FunctionDef (FunctionIdentifier name) args returnType _) hasBody _ =
    (,) name (createFunctionData args returnType hasBody)
createFunctionRecord _ _ state =
    error defaultSourcePos state "Could not create function record." "Internal error."

addFunctionToTable :: Declaration -> ParserState -> ParserState
addFunctionToTable decl@(FunctionDef (FunctionIdentifier _) _ _ body) state =
    state {fst = Map.insert name functionData currentFst}
    where
        hasBody = case body of
            Just body -> True
            Nothing -> False
        (name, functionData) = createFunctionRecord decl hasBody state
        currentFst = fst state

resolveFunctionCalls :: ParserState -> ParserState
resolveFunctionCalls state = foldl resolveCallIfPossible state (unresolvedFunctionCalls state)

canBeResolved :: ParserState -> FunctionCallData -> Bool
canBeResolved state call = case Map.lookup (identifier call) (fst state) of
    Just funcData -> hasBody funcData
    Nothing -> error (pos call) state "This shouldn't happen if the other checks happen first." "Internal error."

resolveCallIfPossible :: ParserState -> FunctionCallData -> ParserState
resolveCallIfPossible state call =
    if canBeResolved state call then
        state { unresolvedFunctionCalls = List.delete call (unresolvedFunctionCalls state) }
    else
        error (pos call) state "Function has a forward declaration but does not have a body." "Consider adding a body to the function."

addUnresolvedCall :: T.Text -> SourcePos -> ParserState -> ParserState
addUnresolvedCall name pos state = state {unresolvedFunctionCalls = newCalls}    
    where
        functionCallData = (FunctionCallData {pos, identifier=name}) 
        calls = unresolvedFunctionCalls state
        newCalls = functionCallData:calls             

hasForwardDecl :: T.Text -> ParserState -> Bool
hasForwardDecl name state = case Map.lookup name (fst state) of
    Just funcData -> not (hasBody funcData)
    Nothing -> False
