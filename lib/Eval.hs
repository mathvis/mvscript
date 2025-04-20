{-# LANGUAGE RankNTypes #-}
module Eval where

import Types
import Control.Applicative

evaluateLogic :: Expression -> Expression
evaluateLogic (Operation op) = case op of
    GreaterThan lhs rhs -> compareValues (>) lhs rhs
    LessThan lhs rhs -> compareValues (<) lhs rhs
    GreaterThanEq lhs rhs -> compareValues (>=) lhs rhs
    LessThanEq lhs rhs -> compareValues (<=) lhs rhs
    Equals lhs rhs -> compareValues (==) lhs rhs
    NotEquals lhs rhs -> compareValues (/=) lhs rhs
evaluateLogic other = other

compareValues :: (forall a. Ord a => a -> a -> Bool) -> Expression -> Expression -> Expression
compareValues op (Type val1) (Type val2) = Type (Bool (compareValuesTyped op val1 val2))
compareValues _ lhs rhs = Operation undefined

compareValuesTyped :: (forall a. Ord a => a -> a -> Bool) -> Type -> Type -> Bool
compareValuesTyped op (Int x) (Int y) = op x y 
compareValuesTyped op (Float x) (Float y) = op x y 
compareValuesTyped op (String x) (String y) = op x y 
compareValuesTyped _ _ _ = undefined

collapseControlFlow :: Declaration -> Declaration
collapseControlFlow (IfBlock condition statement elseBlock) = case evaluateLogic condition of
    Type (Bool True) -> CollapsedControlFlow statement
    Type (Bool False) -> case elseBlock of
        Just (ElseBlock block) -> CollapsedControlFlow block
        _ -> CollapsedControlFlow (Block NoType [])
    _ -> IfBlock condition statement elseBlock


