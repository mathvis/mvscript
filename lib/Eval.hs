{-# LANGUAGE RankNTypes #-}
module Eval where

import Types
import Control.Applicative
import Misc
import Data.Bits ((.|.), (.&.))

compareValues :: (forall a. Ord a => a -> a -> Bool) -> Expression -> Expression -> Expression
compareValues op (Type val1) (Type val2) = Type (Bool (compareValuesTyped op val1 val2))
compareValues _ lhs rhs = Operation undefined

compareValuesTyped :: (forall a. Ord a => a -> a -> Bool) -> Type -> Type -> Bool
compareValuesTyped op (Int x) (Int y) = op x y 
compareValuesTyped op (Float x) (Float y) = op x y 
compareValuesTyped op (String x) (String y) = op x y 
compareValuesTyped _ _ _ = undefined

collapseControlFlow :: Declaration -> Declaration
collapseControlFlow (IfBlock condition statement elseBlock) = case evaluateOperations condition of
    Type (Bool True) -> CollapsedControlFlow statement
    Type (Bool False) -> case elseBlock of
        Just (ElseBlock block) -> CollapsedControlFlow block
        _ -> CollapsedControlFlow (Block NoType [])
    _ -> IfBlock condition statement elseBlock

evaluateOperations :: Expression -> Expression
evaluateOperations (Operation op) = case op of
    Add (Type x) (Type y) -> applyOperator (+) (Type x) (Type y)
    Subtract (Type x) (Type y) -> applyOperator (-) (Type x) (Type y)
    Multiply (Type x) (Type y) -> applyOperator (*) (Type x) (Type y)
    IntDivide (Type x) (Type y) -> applyOperator (\x y -> fromIntegral (intDiv x y)) (Type x) (Type y)
    -- Divide (Type x) (Type y) -> applyOperator (/) (Type x) (Type y)
    -- Modulo (Type x) (Type y) -> applyOperator mod (Type x) (Type y)
    -- BitwiseOr (Type x) (Type y) -> applyOperator (.|.) (Type x) (Type y)
    -- BitwiseAnd (Type x) (Type y) -> applyOperator (.&.) (Type x) (Type y)
    -- BitwiseXor (Type x) (Type y) -> _
    -- BitwiseNot (Type x) -> _
    Negation (Type x) -> applyOperator (-) (Type (Int 0)) (Type x)
    GreaterThan (Type x) (Type y)-> compareValues (>) (Type x) (Type y)
    LessThan (Type x) (Type y) -> compareValues (<) (Type x) (Type y)
    GreaterThanEq (Type x) (Type y) -> compareValues (>=) (Type x) (Type y)
    LessThanEq (Type x) (Type y) -> compareValues (<=) (Type x) (Type y)
    Equals (Type x) (Type y) -> compareValues (==) (Type x) (Type y)
    NotEquals (Type x) (Type y) -> compareValues (/=) (Type x) (Type y)
    _ -> evaluateLogic (Operation op)
evaluateOperations other = other

evaluateLogic :: Expression -> Expression
evaluateLogic (Operation op) = case op of
    Or (Type x) (Type y) -> applyLogic (||) (Type x) (Type y)
    And (Type x) (Type y) -> applyLogic (&&) (Type x) (Type y)
    Not (Type x) -> applyNot (Type x)
    _ -> Operation op
evaluateLogic other = other
    
applyOperator :: (forall a. Real a => a -> a -> a) -> Expression -> Expression -> Expression
applyOperator op (Type (Int val1)) (Type (Int val2)) = Type (Int (op val1 val2))
applyOperator op (Type (Float val1)) (Type (Float val2)) = Type (Float (op val1 val2))
applyOperator op (Type (Bool val1)) (Type (Bool val2)) = Type (Int (op (boolToInt val1) (boolToInt val2)))
applyOperator _ lhs rhs = Operation undefined

applyLogic :: (Bool -> Bool -> Bool) -> Expression -> Expression -> Expression
applyLogic op (Type (Int val1)) (Type (Int val2)) = Type (Bool (op (realToBool val1) (realToBool val2)))
applyLogic op (Type (Float val1)) (Type (Float val2)) = Type (Bool (op (realToBool val1) (realToBool val2)))
applyLogic op (Type (Bool val1)) (Type (Bool val2)) = Type (Bool (op val1 val2))
applyLogic _ lhs rhs = Operation undefined

applyNot :: Expression -> Expression
applyNot (Type (Int val1)) = Type (Bool (not (realToBool val1)))
applyNot (Type (Float val1)) = Type (Bool (not (realToBool val1)))
applyNot (Type (Bool val1)) = Type (Bool (not val1))


