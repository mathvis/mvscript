module SemanticAnalysis.ExpressionSpec (spec) where

import Control.Monad
import qualified Data.Text as T
import qualified ParserTypes as P (Expression (..), Literal (..), Operation (..), fromLiteral)
import SemanticAnalysis (checkExpression)
import SemanticAnalysisTypes (TypedExpression (..), globalEnv)
import qualified SemanticAnalysisTypes as S (ElaboratedType (..))
import SpecUtils (
    createLiteralsForErrorTestExcept,
    dummyPos,
    runCheck,
 )
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = do
    describe "checkExpression typing" $ do
        it "types an int literal correctly" $
            runCheck globalEnv (checkExpression (P.Literal dummyPos (P.Int 5)))
                `shouldSatisfy` exprIsTyped S.IntT
        it "types a string literal correctly" $
            runCheck globalEnv (checkExpression (P.Literal dummyPos (P.String (T.pack "test"))))
                `shouldSatisfy` exprIsTyped S.StringT
        it "types a float literal correctly" $
            runCheck globalEnv (checkExpression (P.Literal dummyPos (P.Float 5.4)))
                `shouldSatisfy` exprIsTyped S.FloatT
        it "types a bool literal correctly" $
            runCheck globalEnv (checkExpression (P.Literal dummyPos (P.Bool True)))
                `shouldSatisfy` exprIsTyped S.BoolT
        it "types a point literal correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    (P.Literal dummyPos (P.Point [P.Literal dummyPos (P.Int 0), P.Literal dummyPos (P.Int 1)]))
                )
                `shouldSatisfy` exprIsTyped S.PointT
        it "types a vector literal correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    (P.Literal dummyPos (P.Vector [P.Literal dummyPos (P.Int 0), P.Literal dummyPos (P.Int 1)]))
                )
                `shouldSatisfy` exprIsTyped S.VectorT
        it "types a matrix literal correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    (P.Literal dummyPos (P.Matrix [P.Literal dummyPos (P.Array [P.Literal dummyPos (P.Int 1)])]))
                )
                `shouldSatisfy` exprIsTyped S.MatrixT
        it "types a vector literal correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    (P.Literal dummyPos (P.Array [P.Literal dummyPos (P.Int 0), P.Literal dummyPos (P.Int 1)]))
                )
                `shouldSatisfy` exprIsTyped (S.ArrayT S.IntT)
        it "types a parenthesized expression correctly" $
            runCheck
                globalEnv
                (checkExpression (P.Parentheses dummyPos (P.Literal dummyPos (P.Int 0))))
                `shouldSatisfy` exprIsTyped S.IntT
        it "types a parenthesized expression correctly 2" $
            runCheck
                globalEnv
                ( checkExpression
                    (P.Parentheses dummyPos (P.Literal dummyPos (P.Array [P.Literal dummyPos (P.Int 0)])))
                )
                `shouldSatisfy` exprIsTyped (S.ArrayT S.IntT)
        it "types an int negation correctly" $
            runCheck
                globalEnv
                (checkExpression (P.Operation dummyPos (P.Negation (P.Literal dummyPos (P.Int 0)))))
                `shouldSatisfy` exprIsTyped S.IntT
        it "types a float negation correctly" $
            runCheck
                globalEnv
                (checkExpression (P.Operation dummyPos (P.Negation (P.Literal dummyPos (P.Float 1.5)))))
                `shouldSatisfy` exprIsTyped S.FloatT
        it "types a bool not correctly" $
            runCheck
                globalEnv
                (checkExpression (P.Operation dummyPos (P.Not (P.Literal dummyPos (P.Bool True)))))
                `shouldSatisfy` exprIsTyped S.BoolT
        it "types a bool and correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    (P.Operation dummyPos (P.And (P.Literal dummyPos (P.Bool True)) (P.Literal dummyPos (P.Bool True))))
                )
                `shouldSatisfy` exprIsTyped S.BoolT
        it "types a bool or correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    (P.Operation dummyPos (P.Or (P.Literal dummyPos (P.Bool True)) (P.Literal dummyPos (P.Bool True))))
                )
                `shouldSatisfy` exprIsTyped S.BoolT
        it "types a int bitwise or correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    (P.Operation dummyPos (P.BitwiseOr (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0))))
                )
                `shouldSatisfy` exprIsTyped S.IntT
        it "types a int bitwise and correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    (P.Operation dummyPos (P.BitwiseAnd (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0))))
                )
                `shouldSatisfy` exprIsTyped S.IntT
        it "types a int bitwise xor correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    (P.Operation dummyPos (P.BitwiseXor (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0))))
                )
                `shouldSatisfy` exprIsTyped S.IntT
        it "types a int greater than correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    (P.Operation dummyPos (P.GreaterThan (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0))))
                )
                `shouldSatisfy` exprIsTyped S.BoolT
        it "types a float greater than correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.GreaterThan (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos (P.Float 0.5)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.BoolT
        it "types a int greater than eq correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    (P.Operation dummyPos (P.GreaterThanEq (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0))))
                )
                `shouldSatisfy` exprIsTyped S.BoolT
        it "types a float greater than eq correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.GreaterThanEq (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos (P.Float 0.5)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.BoolT
        it "types a int less than correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    (P.Operation dummyPos (P.LessThan (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0))))
                )
                `shouldSatisfy` exprIsTyped S.BoolT
        it "types a float less than correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.LessThan (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos (P.Float 0.5)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.BoolT
        it "types a int less than eq correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    (P.Operation dummyPos (P.LessThanEq (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0))))
                )
                `shouldSatisfy` exprIsTyped S.BoolT
        it "types a float less than eq correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.LessThanEq (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos (P.Float 0.5)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.BoolT
        describe "types a equals correctly" $
            forM_ (createLiteralsForErrorTestExcept []) $ \lit ->
                it ("arguments of type " <> show (P.fromLiteral lit)) $
                    runCheck
                        globalEnv
                        (checkExpression (P.Operation dummyPos (P.Equals (P.Literal dummyPos lit) (P.Literal dummyPos lit))))
                        `shouldSatisfy` exprIsTyped
                            S.BoolT
        describe "types a not equals correctly" $
            forM_ (createLiteralsForErrorTestExcept []) $ \lit ->
                it ("arguments of type " <> show (P.fromLiteral lit)) $
                    runCheck
                        globalEnv
                        ( checkExpression
                            (P.Operation dummyPos (P.NotEquals (P.Literal dummyPos lit) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` exprIsTyped
                            S.BoolT
        it "types a int addition correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.Add (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.IntT
        it "types a float addition correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.Add (P.Literal dummyPos (P.Float 0)) (P.Literal dummyPos (P.Float 0)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.FloatT
        it "types a string addition correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.Add (P.Literal dummyPos (P.String (T.pack "test"))) (P.Literal dummyPos (P.String (T.pack "test"))))
                    )
                )
                `shouldSatisfy` exprIsTyped S.StringT
        it "types a int subtraction correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.Subtract (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.IntT
        it "types a float subtraction correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.Subtract (P.Literal dummyPos (P.Float 0)) (P.Literal dummyPos (P.Float 0)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.FloatT
        it "types a int multiplication correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.Multiply (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.IntT
        it "types a float multiplication correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.Multiply (P.Literal dummyPos (P.Float 0)) (P.Literal dummyPos (P.Float 0)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.FloatT
        it "types a int division correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.Divide (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.FloatT
        it "types a float division correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.Divide (P.Literal dummyPos (P.Float 0)) (P.Literal dummyPos (P.Float 0)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.FloatT
        it "types a int int division correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.IntDivide (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.IntT
        it "types a float int division correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.IntDivide (P.Literal dummyPos (P.Float 0)) (P.Literal dummyPos (P.Float 0)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.FloatT
        it "types a int modulo correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.Modulo (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.IntT
        it "types a float modulo correctly" $
            runCheck
                globalEnv
                ( checkExpression
                    ( P.Operation
                        dummyPos
                        (P.Modulo (P.Literal dummyPos (P.Float 0)) (P.Literal dummyPos (P.Float 0)))
                    )
                )
                `shouldSatisfy` exprIsTyped S.FloatT
  where
    exprIsTyped typ (expr, _) = texprType expr == typ
