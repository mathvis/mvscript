module TypeCheck where
import Types 
import Error
import Prelude hiding (error)
import Text.Parsec

--TODO type checking for arrays
valueToType :: Type -> TypeName
valueToType (String _) = StringT
valueToType (Int _) = IntT
valueToType (Float _) = FloatT
valueToType (Bool _) = BoolT
valueToType (Vector _) = VectorT
valueToType (Point _) = PointT
valueToType (Matrix _) = MatrixT

inferVariableType :: SourcePos -> ParserState -> Declaration -> Declaration
inferVariableType _ _ (Variable exp (Just a) val) = Variable exp (Just a) val
inferVariableType _ _ (Variable exp Nothing (Just (Type val))) = Variable exp (Just $ valueToType val) (Just (Type val))
inferVariableType pos state decl = error state pos ("Could not infer type: " ++ show decl) "Internal error."

checkType :: SourcePos -> ParserState -> Declaration -> Declaration
checkType pos state (Variable exp (Just expectedType) (Just (Type val))) = 
    let actualType = valueToType val in
        if expectedType == actualType
            then Variable exp (Just expectedType) (Just (Type val))
            else error state pos ("expected " ++ show expectedType ++ " but got " ++ show actualType) "Consider changing the variable type or declaring a new variable."
checkType pos state _ = error state pos "Declaration is not a variable declaration" "Internal error."

