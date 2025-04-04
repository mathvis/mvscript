module Eval where

import Types

collapseControlFlow :: Declaration -> Declaration
collapseControlFlow (IfBlock condition statement elseBlock) = case condition of
    Type (Bool True) -> CollapsedControlFlow statement
    Type (Bool False) -> case elseBlock of
        Just (ElseBlock block) -> CollapsedControlFlow block
        _ -> CollapsedControlFlow (Block NoType [])
    _ -> IfBlock condition statement elseBlock


