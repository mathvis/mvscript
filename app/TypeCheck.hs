module TypeCheck where
import Types 
import Text.ParserCombinators.Parsec

valueToType :: Type -> TypeName
valueToType (String _) = StringT
valueToType (Int _) = IntT
valueToType (Float _) = FloatT
valueToType (Bool _) = BoolT
valueToType (Vector _) = VectorT
valueToType (Point _) = PointT
valueToType (Matrix _) = MatrixT

inferVariableType :: Declaration -> Declaration
inferVariableType (Variable exp (Just a) val) = Variable exp (Just a) val
inferVariableType (Variable exp Nothing (Just (Type val))) = Variable exp (Just $ valueToType val) (Just (Type val))
inferVariableType decl = error $ "Could not infer type: " ++ show decl

checkType :: Declaration -> Declaration
checkType (Variable exp (Just expectedType) (Just (Type val))) = 
    let actualType = valueToType val in
        if expectedType == actualType
            then Variable exp (Just expectedType) (Just (Type val))
            else error $ "Type mismatch: expected " ++ show expectedType ++ " but got " ++ show actualType
checkType _ = error "Declaration is not a variable declaration"
