module Misc where
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
