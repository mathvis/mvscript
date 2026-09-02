module SemanticAnalysisSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriter)
import qualified Data.Text as T
import qualified ParserTypes as P (
    Expression (..),
    Literal (..),
    Operation (..),
    ParserType (..),
    fromLiteral,
 )
import SemanticAnalysis (checkExpression, checkLiteral, checkOperation)
import SemanticAnalysisTypes (
    Check,
    Env,
    SemanticError (..),
    TypedExpression (..),
    TypedOperation (..),
    globalEnv,
 )
import qualified SemanticAnalysisTypes as S (
    ElaboratedType (..),
    ResolvedExpression (..),
    ResolvedLiteral (..),
    ResolvedOperation (..),
 )
import SpecUtils (dummyPos)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

createLiteral :: P.ParserType -> P.Literal
createLiteral P.IntT = P.Int 0
createLiteral P.FloatT = P.Float 2.5
createLiteral P.BoolT = P.Bool True
createLiteral P.StringT = P.String (T.pack "test")
createLiteral P.VectorT = P.Vector [P.Literal dummyPos (P.Int 0)]
createLiteral P.PointT = P.Point [P.Literal dummyPos (P.Int 0)]
createLiteral P.MatrixT = P.Matrix [P.Literal dummyPos (P.Array [P.Literal dummyPos (P.Int 0)])]
createLiteral (P.ArrayT P.IntT) = P.Array [P.Literal dummyPos (P.Int 0)]
createLiteral _ = error "tried creating literals for invalid types"

createLiteralsForErrorTestExcept :: [P.ParserType] -> [P.Literal]
createLiteralsForErrorTestExcept toExclude = map createLiteral types
  where
    types =
        filter
            (`notElem` toExclude)
            [P.IntT, P.FloatT, P.BoolT, P.StringT, P.VectorT, P.MatrixT, P.PointT, P.ArrayT P.IntT]

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
        describe "does not resolve a non numeric point literal" $
            forM_ (createLiteralsForErrorTestExcept [P.IntT, P.FloatT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd (runCheck globalEnv (checkLiteral dummyPos (P.Point [P.Literal dummyPos lit])))
                        `shouldSatisfy` isTypeMismatch [S.IntT, S.FloatT]
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
        describe "does not resolve a non numeric vector literal" $
            forM_ (createLiteralsForErrorTestExcept [P.IntT, P.FloatT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd (runCheck globalEnv (checkLiteral dummyPos (P.Vector [P.Literal dummyPos lit])))
                        `shouldSatisfy` isTypeMismatch [S.IntT, S.FloatT]
        it "resolves a matrix literal" $
            runCheck
                globalEnv
                (checkLiteral dummyPos (P.Matrix [P.Literal dummyPos (P.Array [P.Literal dummyPos (P.Int 1)])]))
                `shouldBe` ( S.Matrix
                                [ TypedExpression
                                    { texprNode =
                                        S.LiteralExpr
                                            dummyPos
                                            (S.Array [TypedExpression{texprNode = S.LiteralExpr dummyPos (S.Int 1), texprType = S.IntT}]),
                                      texprType = S.ArrayT S.IntT
                                    }
                                ],
                             []
                           )
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
                `shouldBe` [TypeMismatch [S.ArrayT S.IntT, S.ArrayT S.FloatT] S.StringT]
        describe "does not resolve a non numeric array matrix literal" $
            forM_ (createLiteralsForErrorTestExcept [P.IntT, P.FloatT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkLiteral dummyPos (P.Matrix [P.Literal dummyPos (P.Array [P.Literal dummyPos lit])]))
                        )
                        `shouldSatisfy` isTypeMismatch [S.ArrayT S.IntT, S.ArrayT S.FloatT]
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
    describe "checkOperation" $ do
        it "resolves an int negation" $ do
            fst (runCheck globalEnv (checkOperation dummyPos (P.Negation (P.Literal dummyPos (P.Int 1)))))
                `shouldBe` ( TypedOperation
                                { topType = S.IntT,
                                  topNode =
                                    S.Negation (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 1)})
                                }
                           )
        it "resolves a float negation" $ do
            fst (runCheck globalEnv (checkOperation dummyPos (P.Negation (P.Literal dummyPos (P.Float 1.5)))))
                `shouldBe` ( TypedOperation
                                { topType = S.FloatT,
                                  topNode =
                                    S.Negation
                                        (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 1.5)})
                                }
                           )
        describe "does not resolve a non numeric negation" $ do
            forM_ (createLiteralsForErrorTestExcept [P.IntT, P.FloatT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.Negation (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.IntT, S.FloatT]
        it "resolves a bool not" $ do
            fst (runCheck globalEnv (checkOperation dummyPos (P.Not (P.Literal dummyPos (P.Bool True)))))
                `shouldBe` ( TypedOperation
                                { topType = S.BoolT,
                                  topNode =
                                    S.Not
                                        (TypedExpression{texprType = S.BoolT, texprNode = S.LiteralExpr dummyPos (S.Bool True)})
                                }
                           )
        describe "does not resolve a non bool not" $ do
            forM_ (createLiteralsForErrorTestExcept [P.BoolT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.Not (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.BoolT]

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
  where
    exprIsTyped typ (expr, _) = texprType expr == typ
    isTypeMismatch expected [TypeMismatch e _] = e == expected
    isTypeMismatch _ _ = False
