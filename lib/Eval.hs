module Eval where

import Types
import Control.Applicative

evaluateLogic :: Expression -> Expression
evaluateLogic (Operation (GreaterThan (Type (Int lhs)) (Type (Int rhs)))) = Type (Bool (lhs > rhs))
evaluateLogic (Operation (GreaterThan (Type (Float lhs)) (Type (Float rhs)))) = Type (Bool (lhs > rhs))
evaluateLogic (Operation (GreaterThan (Type (String lhs)) (Type (String rhs)))) = Type (Bool (lhs > rhs))
evaluateLogic (Operation (LessThan (Type (Int lhs)) (Type (Int rhs)))) = Type (Bool (lhs < rhs))
evaluateLogic (Operation (LessThan (Type (Float lhs)) (Type (Float rhs)))) = Type (Bool (lhs < rhs))
evaluateLogic (Operation (LessThan (Type (String lhs)) (Type (String rhs)))) = Type (Bool (lhs < rhs))
evaluateLogic (Operation (GreaterThanEq (Type (Int lhs)) (Type (Int rhs)))) = Type (Bool (lhs >= rhs))
evaluateLogic (Operation (GreaterThanEq (Type (Float lhs)) (Type (Float rhs)))) = Type (Bool (lhs >= rhs))
evaluateLogic (Operation (GreaterThanEq (Type (String lhs)) (Type (String rhs)))) = Type (Bool (lhs >= rhs))
evaluateLogic (Operation (LessThanEq (Type (Int lhs)) (Type (Int rhs)))) = Type (Bool (lhs <= rhs))
evaluateLogic (Operation (LessThanEq (Type (Float lhs)) (Type (Float rhs)))) = Type (Bool (lhs <= rhs))
evaluateLogic (Operation (LessThanEq (Type (String lhs)) (Type (String rhs)))) = Type (Bool (lhs <= rhs))
evaluateLogic (Operation (Equals (Type (Int lhs)) (Type (Int rhs)))) = Type (Bool (lhs == rhs))
evaluateLogic (Operation (Equals (Type (Float lhs)) (Type (Float rhs)))) = Type (Bool (lhs == rhs))
evaluateLogic (Operation (Equals (Type (String lhs)) (Type (String rhs)))) = Type (Bool (lhs == rhs))
evaluateLogic (Operation (NotEquals (Type (Int lhs)) (Type (Int rhs)))) = Type (Bool (lhs /= rhs))
evaluateLogic (Operation (NotEquals (Type (Float lhs)) (Type (Float rhs)))) = Type (Bool (lhs /= rhs))
evaluateLogic (Operation (NotEquals (Type (String lhs)) (Type (String rhs)))) = Type (Bool (lhs /= rhs))
evaluateLogic other = other

collapseControlFlow :: Declaration -> Declaration
collapseControlFlow (IfBlock condition statement elseBlock) = case evaluateLogic condition of
    Type (Bool True) -> CollapsedControlFlow statement
    Type (Bool False) -> case elseBlock of
        Just (ElseBlock block) -> CollapsedControlFlow block
        _ -> CollapsedControlFlow (Block NoType [])
    _ -> IfBlock condition statement elseBlock


