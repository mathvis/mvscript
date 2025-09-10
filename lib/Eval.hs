{-# LANGUAGE RankNTypes #-}

module Eval (module Eval) where

import Control.Applicative
import Data.Bits (Bits (complement, xor), (.&.), (.|.))
import Misc
import Text.Parsec
import Types

compareValues :: (forall a. Ord a => a -> a -> Bool) -> Expression -> Expression -> Expression
compareValues op (Type val1) (Type val2) = Type (Bool (compareValuesTyped op val1 val2))
compareValues _ lhs rhs = Operation undefined

compareValuesTyped :: (forall a. Ord a => a -> a -> Bool) -> Type -> Type -> Bool
compareValuesTyped op (Int x) (Int y) = op x y
compareValuesTyped op (Float x) (Float y) = op x y
compareValuesTyped op (String x) (String y) = op x y
compareValuesTyped _ _ _ = undefined

evaluateControlFlow :: MVParser Bool -> Declaration -> MVParser Declaration
evaluateControlFlow collapse (IfBlock condition statement elseBlock) = do
    shouldCollapse <- collapse
    if shouldCollapse
        then do
            bool <- evaluateOperations collapse condition
            case bool of
                Type (Bool True) -> return $ CollapsedControlFlow statement
                Type (Bool False) -> case elseBlock of
                    Just (ElseBlock block) -> return $ CollapsedControlFlow block
                    _ -> return $ CollapsedControlFlow (Block NoType [])
                _ -> return $ IfBlock condition statement elseBlock
        else return (IfBlock condition statement elseBlock)

evaluateOperations :: MVParser Bool -> Expression -> MVParser Expression
evaluateOperations collapse expr@(Operation op) = do
    shouldCollapse <- collapse
    if shouldCollapse
        then case op of
            Add (Type x) (Type y) -> return $ applyOperator (+) (Type x) (Type y)
            Subtract (Type x) (Type y) -> return $ applyOperator (-) (Type x) (Type y)
            Multiply (Type x) (Type y) -> return $ applyOperator (*) (Type x) (Type y)
            IntDivide (Type x) (Type y) -> return $ applyOperator (\x y -> fromIntegral (intDiv x y)) (Type x) (Type y)
            Divide (Type (Float x)) (Type (Float y)) -> return $ Type (Float (x / y))
            Modulo (Type (Int x)) (Type (Int y)) -> return $ Type (Int (mod x y))
            BitwiseOr (Type (Int x)) (Type (Int y)) -> return $ Type (Int (x .|. y))
            BitwiseAnd (Type (Int x)) (Type (Int y)) -> return $ Type (Int (x .&. y))
            BitwiseXor (Type (Int x)) (Type (Int y)) -> return $ Type (Int (xor x y))
            BitwiseNot (Type (Int x)) -> return $ Type (Int (complement x))
            Negation (Type x) -> return $ applyOperator (-) (Type (Int 0)) (Type x)
            GreaterThan (Type x) (Type y) -> return $ compareValues (>) (Type x) (Type y)
            LessThan (Type x) (Type y) -> return $ compareValues (<) (Type x) (Type y)
            GreaterThanEq (Type x) (Type y) -> return $ compareValues (>=) (Type x) (Type y)
            LessThanEq (Type x) (Type y) -> return $ compareValues (<=) (Type x) (Type y)
            Equals (Type x) (Type y) -> return $ compareValues (==) (Type x) (Type y)
            NotEquals (Type x) (Type y) -> return $ compareValues (/=) (Type x) (Type y)
            _ -> return $ evaluateLogic (Operation op)
        else return expr
evaluateOperations _ other = return other

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
