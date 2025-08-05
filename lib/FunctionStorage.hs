{-# LANGUAGE NamedFieldPuns #-}

module FunctionStorage where

import Data.Map as Map hiding (map)
import qualified Data.Text as T
import Types
import Prelude hiding (error, fst)
import Error

checkBody :: Maybe Statement -> Bool
checkBody Nothing = False
checkBody (Just _) = True

argIdsToText :: [(Expression, TypeName)] -> [(T.Text, TypeName)]
argIdsToText = map argIdToText
  where
    argIdToText (VarIdentifier name, typename) = (name, typename)

createFunctionData :: [(Expression, TypeName)] -> TypeName -> Bool -> FunctionData
createFunctionData args returnType hasBody = defaultFunctionData{returnType, arguments = argIdsToText args, hasBody}

createFunctionRecord :: Declaration -> Bool -> ParserState -> (T.Text, FunctionData)
createFunctionRecord (FunctionDef (FunctionIdentifier name) args returnType _) hasBody _ = (,) name (createFunctionData args returnType hasBody)
createFunctionRecord _ _ state = error state defaultSourcePos "Could not create function record." "Internal error."

addFunctionToTable :: Declaration -> Bool -> ParserState -> ParserState
addFunctionToTable decl hasBody state = state{fst = Map.insert name functionData currentFst}
  where
    (name, functionData) = createFunctionRecord decl hasBody state
    currentFst = fst state
