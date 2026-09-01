module SemanticAnalysisSpec (spec) where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriter)
import qualified Data.Text as T
import qualified ParserTypes as P (Expression (..), Literal (..))
import SemanticAnalysis (checkExpression, checkLiteral)
import SemanticAnalysisTypes (Check, Env, SemanticError (..), TypedExpression (..), globalEnv)
import qualified SemanticAnalysisTypes as S (
    ElaboratedType (..),
    ResolvedExpression (..),
    ResolvedLiteral (..),
 )
import SpecUtils (dummyPos)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

runCheck :: Env -> Check a -> (a, [SemanticError])
runCheck env m = runWriter (runReaderT m env)

spec :: Spec
spec = do
    describe "checkLiteral" $ do
        it "resolves an int literal" $
            runCheck globalEnv (checkLiteral dummyPos (P.Int 5)) `shouldBe` (S.Int 5, [])
        it "resolves a string literal" $
            runCheck globalEnv (checkLiteral dummyPos (P.String (T.pack "test")))
                `shouldBe` (S.String (T.pack "test"), [])
        it "resolves a float literal" $
            runCheck globalEnv (checkLiteral dummyPos (P.Float 5.4)) `shouldBe` (S.Float 5.4, [])
        it "resolves a bool literal" $
            runCheck globalEnv (checkLiteral dummyPos (P.Bool True)) `shouldBe` (S.Bool True, [])
        it "resolves a point literal" $
            runCheck
                globalEnv
                (checkLiteral dummyPos (P.Point [P.Literal dummyPos (P.Int 0), P.Literal dummyPos (P.Int 1)]))
                `shouldBe` ( S.Point
                                [ TypedExpression{texprNode = S.LiteralExpr dummyPos (S.Int 0), texprType = S.IntT},
                                  TypedExpression{texprNode = S.LiteralExpr dummyPos (S.Int 1), texprType = S.IntT}
                                ],
                             []
                           )
        it "does not resolve an empty point literal" $
            snd (runCheck globalEnv (checkLiteral dummyPos (P.Point []))) `shouldBe` [EmptyMVContainer]
        it "does not resolve a mixed type point literal" $
            snd
                ( runCheck
                    globalEnv
                    (checkLiteral dummyPos (P.Point [P.Literal dummyPos (P.Int 0), P.Literal dummyPos (P.Float 1.3)]))
                )
                `shouldBe` [TypeMismatch [S.IntT] S.FloatT]
        it "does not resolve a non numeric point literal" $
            snd
                (runCheck globalEnv (checkLiteral dummyPos (P.Point [P.Literal dummyPos (P.String (T.pack "test"))])))
                `shouldBe` [TypeMismatch [S.IntT, S.FloatT] S.StringT]
        it "resolves a vector literal" $
            runCheck
                globalEnv
                (checkLiteral dummyPos (P.Vector [P.Literal dummyPos (P.Int 0), P.Literal dummyPos (P.Int 1)]))
                `shouldBe` ( S.Vector
                                [ TypedExpression{texprNode = S.LiteralExpr dummyPos (S.Int 0), texprType = S.IntT},
                                  TypedExpression{texprNode = S.LiteralExpr dummyPos (S.Int 1), texprType = S.IntT}
                                ],
                             []
                           )
        it "does not resolve an empty vector literal" $
            snd (runCheck globalEnv (checkLiteral dummyPos (P.Vector []))) `shouldBe` [EmptyMVContainer]
        it "does not resolve a mixed type vector literal" $
            snd
                ( runCheck
                    globalEnv
                    (checkLiteral dummyPos (P.Vector [P.Literal dummyPos (P.Int 0), P.Literal dummyPos (P.Float 1.3)]))
                )
                `shouldBe` [TypeMismatch [S.IntT] S.FloatT]
        it "does not resolve a non numeric vector literal" $
            snd
                ( runCheck
                    globalEnv
                    (checkLiteral dummyPos (P.Vector [P.Literal dummyPos (P.String (T.pack "test"))]))
                )
                `shouldBe` [TypeMismatch [S.IntT, S.FloatT] S.StringT]
        it "resolves a matrix literal" $
            runCheck
                globalEnv
                (checkLiteral dummyPos (P.Matrix [P.Literal dummyPos (P.Array [P.Literal dummyPos (P.Int 1)])]))
                `shouldBe` ( S.Matrix
                                [ TypedExpression
                                    { texprNode =
                                        S.LiteralExpr
                                            dummyPos
                                            (S.Array [TypedExpression{texprNode = (S.LiteralExpr dummyPos (S.Int 1)), texprType = S.IntT}]),
                                      texprType = S.ArrayT S.IntT
                                    }
                                ],
                             []
                           )
        it "does not resolve an empty matrix literal" $
            snd (runCheck globalEnv (checkLiteral dummyPos (P.Matrix []))) `shouldBe` [EmptyMVContainer]
        it "does not resolve an empty matrix literal" $
            snd (runCheck globalEnv (checkLiteral dummyPos (P.Matrix []))) `shouldBe` [EmptyMVContainer]
        it "does not resolve a mixed type matrix literal" $
            snd
                ( runCheck
                    globalEnv
                    ( checkLiteral
                        dummyPos
                        ( P.Matrix
                            [ P.Literal dummyPos (P.Array [P.Literal dummyPos (P.Int 0)]),
                              P.Literal dummyPos (P.Array [P.Literal dummyPos (P.Float 0.5)])
                            ]
                        )
                    )
                )
                `shouldBe` [TypeMismatch [S.ArrayT S.IntT] (S.ArrayT S.FloatT)]
        it "does not resolve a non array matrix literal" $
            snd
                ( runCheck
                    globalEnv
                    (checkLiteral dummyPos (P.Matrix [P.Literal dummyPos (P.String (T.pack "test"))]))
                )
                `shouldBe` [TypeMismatch [(S.ArrayT S.IntT), (S.ArrayT S.FloatT)] S.StringT]
        it "resolves an array literal" $
            runCheck
                globalEnv
                (checkLiteral dummyPos (P.Array [P.Literal dummyPos (P.Int 0), P.Literal dummyPos (P.Int 1)]))
                `shouldBe` ( S.Array
                                [ TypedExpression{texprNode = S.LiteralExpr dummyPos (S.Int 0), texprType = S.IntT},
                                  TypedExpression{texprNode = S.LiteralExpr dummyPos (S.Int 1), texprType = S.IntT}
                                ],
                             []
                           )
        it "does not resolve a mixed type array literal" $
            snd
                ( runCheck
                    globalEnv
                    (checkLiteral dummyPos (P.Array [P.Literal dummyPos (P.Int 0), P.Literal dummyPos (P.Bool True)]))
                )
                `shouldBe` [TypeMismatch [S.IntT] S.BoolT]
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
  where
    exprIsTyped typ (expr, _) = texprType expr == typ
