module TypeCheck where
import Types 
import Text.ParserCombinators.Parsec

inferVariableType :: Declaration -> Declaration
inferVariableType (Variable exp (Just a) val) = Variable exp (Just a) val
inferVariableType (Variable exp Nothing (Just (Type val))) =
    case val of
        String _ -> Variable exp (Just StringT) (Just (Type val))
        Int _ -> Variable exp (Just IntT) (Just (Type val))
        Float _ -> Variable exp (Just FloatT) (Just (Type val))
        Bool _ -> Variable exp (Just BoolT) (Just (Type val))
        Vector _ -> Variable exp (Just VectorT) (Just (Type val))
        Point _ -> Variable exp (Just PointT) (Just (Type val))
        Matrix _ -> Variable exp (Just MatrixT) (Just (Type val))
inferVariableType _ = error "Declaration is not a variable declaration"

checkType :: Declaration -> Declaration
checkType (Variable exp (Just t) (Just (Type val))) = case (t, val) of
    (IntT, Int a) -> Variable exp (Just t) (Just (Type val))
    (StringT, String a) -> Variable exp (Just t) (Just (Type val))
    (FloatT, Float a) -> Variable exp (Just t) (Just (Type val))
    (BoolT, Bool a) -> Variable exp (Just t) (Just (Type val))
    (PointT, Point a) -> Variable exp (Just t) (Just (Type val))
    (VectorT, Vector a) -> Variable exp (Just t) (Just (Type val))
    (MatrixT, Matrix a) -> Variable exp (Just t) (Just (Type val))
    (_, _) -> error "Specified type is not the initialized variable's type"
checkType _ = error "Declaration is not a variable declaration"
