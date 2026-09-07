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
    fromParserLiteral,
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
        it "resolves a bool and" $ do
            fst
                ( runCheck
                    globalEnv
                    ( checkOperation
                        dummyPos
                        (P.And (P.Literal dummyPos (P.Bool True)) (P.Literal dummyPos (P.Bool True)))
                    )
                )
                `shouldBe` ( TypedOperation
                                { topType = S.BoolT,
                                  topNode =
                                    S.And
                                        (TypedExpression{texprType = S.BoolT, texprNode = S.LiteralExpr dummyPos (S.Bool True)})
                                        (TypedExpression{texprType = S.BoolT, texprNode = S.LiteralExpr dummyPos (S.Bool True)})
                                }
                           )
        describe "does not resolve a non bool and" $ do
            forM_ (createLiteralsForErrorTestExcept [P.BoolT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.And (P.Literal dummyPos lit) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.BoolT]
        describe "does not resolve a mixed type and" $ do
            forM_ (createLiteralsForErrorTestExcept [P.BoolT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.And (P.Literal dummyPos (P.Bool True)) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.BoolT]
        it "resolves a bool or" $ do
            fst
                ( runCheck
                    globalEnv
                    ( checkOperation
                        dummyPos
                        (P.Or (P.Literal dummyPos (P.Bool True)) (P.Literal dummyPos (P.Bool True)))
                    )
                )
                `shouldBe` ( TypedOperation
                                { topType = S.BoolT,
                                  topNode =
                                    S.Or
                                        (TypedExpression{texprType = S.BoolT, texprNode = S.LiteralExpr dummyPos (S.Bool True)})
                                        (TypedExpression{texprType = S.BoolT, texprNode = S.LiteralExpr dummyPos (S.Bool True)})
                                }
                           )
        describe "does not resolve a non bool or" $ do
            forM_ (createLiteralsForErrorTestExcept [P.BoolT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.Or (P.Literal dummyPos lit) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.BoolT]
        describe "does not resolve a mixed type or" $ do
            forM_ (createLiteralsForErrorTestExcept [P.BoolT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.Or (P.Literal dummyPos (P.Bool True)) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.BoolT]
        it "resolves a int bitwise and" $ do
            fst
                ( runCheck
                    globalEnv
                    ( checkOperation
                        dummyPos
                        (P.BitwiseAnd (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                    )
                )
                `shouldBe` ( TypedOperation
                                { topType = S.IntT,
                                  topNode =
                                    S.BitwiseAnd
                                        (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                        (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                }
                           )
        describe "does not resolve a non int bitwise and" $ do
            forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.BitwiseAnd (P.Literal dummyPos lit) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.IntT]
        describe "does not resolve a mixed type bitwise and" $ do
            forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.BitwiseAnd (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.IntT]
        it "resolves a int bitwise xor" $ do
            fst
                ( runCheck
                    globalEnv
                    ( checkOperation
                        dummyPos
                        (P.BitwiseXor (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                    )
                )
                `shouldBe` ( TypedOperation
                                { topType = S.IntT,
                                  topNode =
                                    S.BitwiseXor
                                        (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                        (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                }
                           )
        describe "does not resolve a non int bitwise xor" $ do
            forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.BitwiseXor (P.Literal dummyPos lit) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.IntT]
        describe "does not resolve a mixed type bitwise xor" $ do
            forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.BitwiseXor (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.IntT]
        it "resolves a int bitwise or" $ do
            fst
                ( runCheck
                    globalEnv
                    ( checkOperation
                        dummyPos
                        (P.BitwiseOr (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                    )
                )
                `shouldBe` ( TypedOperation
                                { topType = S.IntT,
                                  topNode =
                                    S.BitwiseOr
                                        (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                        (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                }
                           )
        describe "does not resolve a non int bitwise or" $ do
            forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.BitwiseOr (P.Literal dummyPos lit) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.IntT]
        describe "does not resolve a mixed type bitwise or" $ do
            forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.BitwiseOr (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.IntT]
        it "resolves a int greater than" $ do
            fst
                ( runCheck
                    globalEnv
                    ( checkOperation
                        dummyPos
                        (P.GreaterThan (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                    )
                )
                `shouldBe` ( TypedOperation
                                { topType = S.BoolT,
                                  topNode =
                                    S.GreaterThan
                                        (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                        (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                }
                           )
        it "resolves a float greater than" $ do
            fst
                ( runCheck
                    globalEnv
                    ( checkOperation
                        dummyPos
                        (P.GreaterThan (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos (P.Float 0.5)))
                    )
                )
                `shouldBe` ( TypedOperation
                                { topType = S.BoolT,
                                  topNode =
                                    S.GreaterThan
                                        (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                                        (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                                }
                           )
        describe "does not resolve a non numeric greater than" $ do
            forM_ (createLiteralsForErrorTestExcept [P.FloatT, P.IntT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.GreaterThan (P.Literal dummyPos lit) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.IntT, S.FloatT]
        describe "does not resolve a mixed type greater than" $ do
            forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit) <> " with int") $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.GreaterThan (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.IntT]
            forM_ (createLiteralsForErrorTestExcept [P.FloatT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit) <> " with float") $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.GreaterThan (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.FloatT]
        it "resolves a int greater than eq" $ do
            fst
                ( runCheck
                    globalEnv
                    ( checkOperation
                        dummyPos
                        (P.GreaterThanEq (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                    )
                )
                `shouldBe` ( TypedOperation
                                { topType = S.BoolT,
                                  topNode =
                                    S.GreaterThanEq
                                        (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                        (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                }
                           )
        it "resolves a float greater than eq" $ do
            fst
                ( runCheck
                    globalEnv
                    ( checkOperation
                        dummyPos
                        (P.GreaterThanEq (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos (P.Float 0.5)))
                    )
                )
                `shouldBe` ( TypedOperation
                                { topType = S.BoolT,
                                  topNode =
                                    S.GreaterThanEq
                                        (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                                        (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                                }
                           )
        describe "does not resolve a non numeric greater than eq" $ do
            forM_ (createLiteralsForErrorTestExcept [P.FloatT, P.IntT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.GreaterThanEq (P.Literal dummyPos lit) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.IntT, S.FloatT]
        describe "does not resolve a mixed type greater than eq" $ do
            forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit) <> " with int") $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.GreaterThanEq (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.IntT]
            forM_ (createLiteralsForErrorTestExcept [P.FloatT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit) <> " with float") $
                    snd
                        ( runCheck
                            globalEnv
                            ( checkOperation
                                dummyPos
                                (P.GreaterThanEq (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos lit))
                            )
                        )
                        `shouldSatisfy` isTypeMismatch [S.FloatT]
        it "resolves a int less than" $ do
            fst
                ( runCheck
                    globalEnv
                    ( checkOperation
                        dummyPos
                        (P.LessThan (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                    )
                )
                `shouldBe` ( TypedOperation
                                { topType = S.BoolT,
                                  topNode =
                                    S.LessThan
                                        (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                        (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                }
                           )
        it "resolves a float less than" $ do
            fst
                ( runCheck
                    globalEnv
                    ( checkOperation
                        dummyPos
                        (P.LessThan (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos (P.Float 0.5)))
                    )
                )
                `shouldBe` ( TypedOperation
                                { topType = S.BoolT,
                                  topNode =
                                    S.LessThan
                                        (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                                        (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                                }
                           )
        describe "does not resolve a non numeric less than" $ do
            forM_ (createLiteralsForErrorTestExcept [P.FloatT, P.IntT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.LessThan (P.Literal dummyPos lit) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.IntT, S.FloatT]
        describe "does not resolve a mixed type less than" $ do
            forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit) <> " with int") $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.LessThan (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.IntT]
            forM_ (createLiteralsForErrorTestExcept [P.FloatT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit) <> " with float") $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.LessThan (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.FloatT]
        it "resolves a int less than eq" $ do
            fst
                ( runCheck
                    globalEnv
                    ( checkOperation
                        dummyPos
                        (P.LessThanEq (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                    )
                )
                `shouldBe` ( TypedOperation
                                { topType = S.BoolT,
                                  topNode =
                                    S.LessThanEq
                                        (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                        (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                }
                           )
        it "resolves a float less than eq" $ do
            fst
                ( runCheck
                    globalEnv
                    ( checkOperation
                        dummyPos
                        (P.LessThanEq (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos (P.Float 0.5)))
                    )
                )
                `shouldBe` ( TypedOperation
                                { topType = S.BoolT,
                                  topNode =
                                    S.LessThanEq
                                        (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                                        (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                                }
                           )
        describe "does not resolve a non numeric less than eq" $ do
            forM_ (createLiteralsForErrorTestExcept [P.FloatT, P.IntT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.LessThanEq (P.Literal dummyPos lit) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.IntT, S.FloatT]
        describe "does not resolve a mixed type less than eq" $ do
            forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit) <> " with int") $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.LessThanEq (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos lit)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.IntT]
            forM_ (createLiteralsForErrorTestExcept [P.FloatT]) $ \lit ->
                it ("rejects " <> show (P.fromLiteral lit) <> " with float") $
                    snd
                        ( runCheck
                            globalEnv
                            ( checkOperation
                                dummyPos
                                (P.LessThanEq (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos lit))
                            )
                        )
                        `shouldSatisfy` isTypeMismatch [S.FloatT]
    describe "resolves an equals for matching types" $
        forM_ (createLiteralsForErrorTestExcept []) $ \lit ->
            it ("accepts two " <> show (P.fromLiteral lit)) $
                fst
                    ( runCheck
                        globalEnv
                        (checkOperation dummyPos (P.Equals (P.Literal dummyPos lit) (P.Literal dummyPos lit)))
                    )
                    `shouldSatisfy` \top -> topType top == S.BoolT

    describe "resolves a not equals for matching types" $
        forM_ (createLiteralsForErrorTestExcept []) $ \lit ->
            it ("accepts two " <> show (P.fromLiteral lit)) $
                fst
                    ( runCheck
                        globalEnv
                        (checkOperation dummyPos (P.NotEquals (P.Literal dummyPos lit) (P.Literal dummyPos lit)))
                    )
                    `shouldSatisfy` \top -> topType top == S.BoolT
    describe "does not resolve an equals with mismatched types" $
        forM_ (createLiteralsForErrorTestExcept []) $ \lit1 ->
            forM_ (createLiteralsForErrorTestExcept [P.fromLiteral lit1]) $ \lit2 ->
                it ("rejects " <> show (P.fromLiteral lit1) <> " vs " <> show (P.fromLiteral lit2)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.Equals (P.Literal dummyPos lit1) (P.Literal dummyPos lit2)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.fromParserLiteral lit1]
    describe "does not resolve a not equals with mismatched types" $
        forM_ (createLiteralsForErrorTestExcept []) $ \lit1 ->
            forM_ (createLiteralsForErrorTestExcept [P.fromLiteral lit1]) $ \lit2 ->
                it ("rejects " <> show (P.fromLiteral lit1) <> " vs " <> show (P.fromLiteral lit2)) $
                    snd
                        ( runCheck
                            globalEnv
                            (checkOperation dummyPos (P.NotEquals (P.Literal dummyPos lit1) (P.Literal dummyPos lit2)))
                        )
                        `shouldSatisfy` isTypeMismatch [S.fromParserLiteral lit1]

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
  where
    exprIsTyped typ (expr, _) = texprType expr == typ
    isTypeMismatch expected [TypeMismatch e _] = e == expected
    isTypeMismatch _ _ = False
