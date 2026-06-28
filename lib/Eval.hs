{-# LANGUAGE RankNTypes #-}

module Eval (module Eval) where

import Control.Applicative
import Data.Bits (Bits (complement, xor), (.&.), (.|.))
import Misc
import Types

compareValues :: (forall a. Ord a => a -> a -> Bool) -> Expression -> Expression -> Expression
compareValues op (Literal val1) (Literal val2) = Literal (Bool (compareValuesTyped op val1 val2))
compareValues _ lhs rhs = Operation undefined

compareValuesTyped :: (forall a. Ord a => a -> a -> Bool) -> Literal -> Literal -> Bool
compareValuesTyped op (Int x) (Int y) = op x y
compareValuesTyped op (Float x) (Float y) = op x y
compareValuesTyped op (String x) (String y) = op x y
compareValuesTyped _ _ _ = undefined

evaluateControlFlow :: MVParser Bool -> Statement -> MVParser Statement
evaluateControlFlow collapse (IfStmt (Just condition) statement elseBlock) = do
    shouldCollapse <- collapse
    if shouldCollapse
        then case foldExpression condition of
            Literal (Bool True)  -> return $ CollapsedControlFlow statement
            Literal (Bool False) -> case elseBlock of
                Just (Stmt (ElseStmt block)) -> return $ CollapsedControlFlow block
                _                      -> return $ CollapsedControlFlow (Block NoType [])
            _                 -> return $ IfStmt (Just condition) statement elseBlock
        else return (IfStmt (Just condition) statement elseBlock)
evaluateControlFlow _ other = return other

foldExpression :: Expression -> Expression
foldExpression expr@(Operation op) = case op of
    Add (Literal x) (Literal y)                       -> applyOperator (+) (Literal x) (Literal y)
    Subtract (Literal x) (Literal y)                  -> applyOperator (-) (Literal x) (Literal y)
    Multiply (Literal x) (Literal y)                  -> applyOperator (*) (Literal x) (Literal y)
    IntDivide (Literal x) (Literal y)                 -> applyOperator (\a b -> fromIntegral (intDiv a b)) (Literal x) (Literal y)
    Divide (Literal (Float x)) (Literal (Float y))    -> Literal (Float (x / y))
    Modulo (Literal (Int x)) (Literal (Int y))        -> Literal (Int (mod x y))
    BitwiseOr (Literal (Int x)) (Literal (Int y))     -> Literal (Int (x .|. y))
    BitwiseAnd (Literal (Int x)) (Literal (Int y))    -> Literal (Int (x .&. y))
    BitwiseXor (Literal (Int x)) (Literal (Int y))    -> Literal (Int (xor x y))
    BitwiseNot (Literal (Int x))                   -> Literal (Int (complement x))
    Negation (Literal x)                           -> applyOperator (-) (Literal (Int 0)) (Literal x)
    GreaterThan (Literal x) (Literal y)               -> compareValues (>) (Literal x) (Literal y)
    LessThan (Literal x) (Literal y)                  -> compareValues (<) (Literal x) (Literal y)
    GreaterThanEq (Literal x) (Literal y)             -> compareValues (>=) (Literal x) (Literal y)
    LessThanEq (Literal x) (Literal y)                -> compareValues (<=) (Literal x) (Literal y)
    Equals (Literal x) (Literal y)                    -> compareValues (==) (Literal x) (Literal y)
    NotEquals (Literal x) (Literal y)                 -> compareValues (/=) (Literal x) (Literal y)
    Not (Literal (Bool x))                         -> Literal (Bool (not x))
    And (Literal (Bool x)) (Literal (Bool y))         -> Literal (Bool (x && y))
    Or (Literal (Bool x)) (Literal (Bool y))          -> Literal (Bool (x || y))
    _                                           -> evaluateLogic expr
foldExpression other = other

evaluateLogic :: Expression -> Expression
evaluateLogic (Operation op) = case op of
    Or (Literal x) (Literal y) -> applyLogic (||) (Literal x) (Literal y)
    And (Literal x) (Literal y) -> applyLogic (&&) (Literal x) (Literal y)
    Not (Literal x) -> applyNot (Literal x)
    _ -> Operation op
evaluateLogic other = other

applyOperator :: (forall a. Real a => a -> a -> a) -> Expression -> Expression -> Expression
applyOperator op (Literal (Int val1)) (Literal (Int val2)) = Literal (Int (op val1 val2))
applyOperator op (Literal (Float val1)) (Literal (Float val2)) = Literal (Float (op val1 val2))
applyOperator op (Literal (Bool val1)) (Literal (Bool val2)) = Literal (Int (op (boolToInt val1) (boolToInt val2)))
applyOperator _ lhs rhs = Operation undefined

applyLogic :: (Bool -> Bool -> Bool) -> Expression -> Expression -> Expression
applyLogic op (Literal (Int val1)) (Literal (Int val2)) = Literal (Bool (op (realToBool val1) (realToBool val2)))
applyLogic op (Literal (Float val1)) (Literal (Float val2)) = Literal (Bool (op (realToBool val1) (realToBool val2)))
applyLogic op (Literal (Bool val1)) (Literal (Bool val2)) = Literal (Bool (op val1 val2))
applyLogic _ lhs rhs = Operation undefined

applyNot :: Expression -> Expression
applyNot (Literal (Int val1)) = Literal (Bool (not (realToBool val1)))
applyNot (Literal (Float val1)) = Literal (Bool (not (realToBool val1)))
applyNot (Literal (Bool val1)) = Literal (Bool (not val1))
