module SemanticAnalysis.OperationSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Text as T
import qualified ParserTypes as P (
    Expression (..),
    Literal (..),
    Operation (..),
    ParserType (..),
    fromLiteral,
 )
import SemanticAnalysis (checkOperation)
import SemanticAnalysisTypes (
    SemanticError (TypeMismatch),
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
import SpecUtils (createLiteralsForErrorTestExcept, dummyPos, runCheck)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
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
    it "resolves a int addition" $ do
        fst
            ( runCheck
                globalEnv
                ( checkOperation
                    dummyPos
                    (P.Add (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                )
            )
            `shouldBe` ( TypedOperation
                            { topType = S.IntT,
                              topNode =
                                S.Add
                                    (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                    (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                            }
                       )
    it "resolves a float addition" $ do
        fst
            ( runCheck
                globalEnv
                ( checkOperation
                    dummyPos
                    (P.Add (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos (P.Float 0.5)))
                )
            )
            `shouldBe` ( TypedOperation
                            { topType = S.FloatT,
                              topNode =
                                S.Add
                                    (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                                    (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                            }
                       )
    it "resolves a string addition" $ do
        fst
            ( runCheck
                globalEnv
                ( checkOperation
                    dummyPos
                    ( P.Add
                        (P.Literal dummyPos (P.String (T.pack "test")))
                        (P.Literal dummyPos (P.String (T.pack "test")))
                    )
                )
            )
            `shouldBe` ( TypedOperation
                            { topType = S.StringT,
                              topNode =
                                S.Add
                                    ( TypedExpression
                                        { texprType = S.StringT,
                                          texprNode = S.LiteralExpr dummyPos (S.String (T.pack "test"))
                                        }
                                    )
                                    ( TypedExpression
                                        { texprType = S.StringT,
                                          texprNode = S.LiteralExpr dummyPos (S.String (T.pack "test"))
                                        }
                                    )
                            }
                       )
    it "resolves a int subtraction" $ do
        fst
            ( runCheck
                globalEnv
                ( checkOperation
                    dummyPos
                    (P.Subtract (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                )
            )
            `shouldBe` ( TypedOperation
                            { topType = S.IntT,
                              topNode =
                                S.Subtract
                                    (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                    (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                            }
                       )
    it "resolves a float subtraction" $ do
        fst
            ( runCheck
                globalEnv
                ( checkOperation
                    dummyPos
                    (P.Subtract (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos (P.Float 0.5)))
                )
            )
            `shouldBe` ( TypedOperation
                            { topType = S.FloatT,
                              topNode =
                                S.Subtract
                                    (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                                    (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                            }
                       )
    it "resolves a int multiplication" $ do
        fst
            ( runCheck
                globalEnv
                ( checkOperation
                    dummyPos
                    (P.Multiply (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                )
            )
            `shouldBe` ( TypedOperation
                            { topType = S.IntT,
                              topNode =
                                S.Multiply
                                    (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                    (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                            }
                       )
    it "resolves a float multiplication" $ do
        fst
            ( runCheck
                globalEnv
                ( checkOperation
                    dummyPos
                    (P.Multiply (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos (P.Float 0.5)))
                )
            )
            `shouldBe` ( TypedOperation
                            { topType = S.FloatT,
                              topNode =
                                S.Multiply
                                    (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                                    (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                            }
                       )
    it "resolves a int division" $ do
        fst
            ( runCheck
                globalEnv
                ( checkOperation
                    dummyPos
                    (P.Divide (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                )
            )
            `shouldBe` ( TypedOperation
                            { topType = S.FloatT,
                              topNode =
                                S.Divide
                                    (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                    (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                            }
                       )
    it "resolves a float division" $ do
        fst
            ( runCheck
                globalEnv
                ( checkOperation
                    dummyPos
                    (P.Divide (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos (P.Float 0.5)))
                )
            )
            `shouldBe` ( TypedOperation
                            { topType = S.FloatT,
                              topNode =
                                S.Divide
                                    (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                                    (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                            }
                       )
    it "resolves a int int division" $ do
        fst
            ( runCheck
                globalEnv
                ( checkOperation
                    dummyPos
                    (P.IntDivide (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                )
            )
            `shouldBe` ( TypedOperation
                            { topType = S.IntT,
                              topNode =
                                S.IntDivide
                                    (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                    (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                            }
                       )
    it "resolves a float int division" $ do
        fst
            ( runCheck
                globalEnv
                ( checkOperation
                    dummyPos
                    (P.IntDivide (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos (P.Float 0.5)))
                )
            )
            `shouldBe` ( TypedOperation
                            { topType = S.FloatT,
                              topNode =
                                S.IntDivide
                                    (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                                    (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                            }
                       )
    it "resolves a int modulo" $ do
        fst
            ( runCheck
                globalEnv
                ( checkOperation
                    dummyPos
                    (P.Modulo (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos (P.Int 0)))
                )
            )
            `shouldBe` ( TypedOperation
                            { topType = S.IntT,
                              topNode =
                                S.Modulo
                                    (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                                    (TypedExpression{texprType = S.IntT, texprNode = S.LiteralExpr dummyPos (S.Int 0)})
                            }
                       )
    it "resolves a float modulo" $ do
        fst
            ( runCheck
                globalEnv
                ( checkOperation
                    dummyPos
                    (P.Modulo (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos (P.Float 0.5)))
                )
            )
            `shouldBe` ( TypedOperation
                            { topType = S.FloatT,
                              topNode =
                                S.Modulo
                                    (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                                    (TypedExpression{texprType = S.FloatT, texprNode = S.LiteralExpr dummyPos (S.Float 0.5)})
                            }
                       )
    describe "does not resolve an addition with mismatched types" $ do
        forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
            it ("rejects int vs " <> show (P.fromLiteral lit))
                $ snd
                    ( runCheck
                        globalEnv
                        (checkOperation dummyPos (P.Add (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos lit)))
                    )
                    `shouldSatisfy` isTypeMismatch
                        [S.IntT]
        forM_ (createLiteralsForErrorTestExcept [P.FloatT]) $ \lit ->
            it ("rejects float vs " <> show (P.fromLiteral lit))
                $ snd
                    ( runCheck
                        globalEnv
                        (checkOperation dummyPos (P.Add (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos lit)))
                    )
                `shouldSatisfy` isTypeMismatch [S.FloatT]
        forM_ (createLiteralsForErrorTestExcept [P.StringT]) $ \lit ->
                            it ("rejects string vs " <> show (P.fromLiteral lit)) $
                                snd
                                    ( runCheck
                                        globalEnv
                                        ( checkOperation
                                            dummyPos
                                            (P.Add (P.Literal dummyPos (P.String (T.pack "test"))) (P.Literal dummyPos lit))
                                        )
                                    )
                                    `shouldSatisfy` isTypeMismatch [S.StringT]
    describe "does not resolve a subtraction with mismatched types" $ do
        forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
            it ("rejects int vs " <> show (P.fromLiteral lit))
                $ snd
                    ( runCheck
                        globalEnv
                        (checkOperation dummyPos (P.Subtract (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos lit)))
                    )
                    `shouldSatisfy` isTypeMismatch
                        [S.IntT]
        forM_ (createLiteralsForErrorTestExcept [P.FloatT]) $ \lit ->
            it ("rejects float vs " <> show (P.fromLiteral lit))
                $ snd
                    ( runCheck
                        globalEnv
                        (checkOperation dummyPos (P.Subtract (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos lit)))
                    )
                `shouldSatisfy` isTypeMismatch [S.FloatT]
    describe "does not resolve a multiplication with mismatched types" $ do
        forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
            it ("rejects int vs " <> show (P.fromLiteral lit))
                $ snd
                    ( runCheck
                        globalEnv
                        (checkOperation dummyPos (P.Multiply (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos lit)))
                    )
                    `shouldSatisfy` isTypeMismatch
                        [S.IntT]
        forM_ (createLiteralsForErrorTestExcept [P.FloatT]) $ \lit ->
            it ("rejects float vs " <> show (P.fromLiteral lit))
                $ snd
                    ( runCheck
                        globalEnv
                        (checkOperation dummyPos (P.Multiply (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos lit)))
                    )
                `shouldSatisfy` isTypeMismatch [S.FloatT]
    describe "does not resolve a division with mismatched types" $ do
        forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
            it ("rejects int vs " <> show (P.fromLiteral lit))
                $ snd
                    ( runCheck
                        globalEnv
                        (checkOperation dummyPos (P.Divide (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos lit)))
                    )
                    `shouldSatisfy` isTypeMismatch
                        [S.IntT]
        forM_ (createLiteralsForErrorTestExcept [P.FloatT]) $ \lit ->
            it ("rejects float vs " <> show (P.fromLiteral lit))
                $ snd
                    ( runCheck
                        globalEnv
                        (checkOperation dummyPos (P.Divide (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos lit)))
                    )
                `shouldSatisfy` isTypeMismatch [S.FloatT]
    describe "does not resolve a int division with mismatched types" $ do
        forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
            it ("rejects int vs " <> show (P.fromLiteral lit))
                $ snd
                    ( runCheck
                        globalEnv
                        (checkOperation dummyPos (P.IntDivide (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos lit)))
                    )
                    `shouldSatisfy` isTypeMismatch
                        [S.IntT]
        forM_ (createLiteralsForErrorTestExcept [P.FloatT]) $ \lit ->
            it ("rejects float vs " <> show (P.fromLiteral lit))
                $ snd
                    ( runCheck
                        globalEnv
                        (checkOperation dummyPos (P.IntDivide (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos lit)))
                    )
                `shouldSatisfy` isTypeMismatch [S.FloatT]
    describe "does not resolve a modulo with mismatched types" $ do
        forM_ (createLiteralsForErrorTestExcept [P.IntT]) $ \lit ->
            it ("rejects int vs " <> show (P.fromLiteral lit))
                $ snd
                    ( runCheck
                        globalEnv
                        (checkOperation dummyPos (P.Modulo (P.Literal dummyPos (P.Int 0)) (P.Literal dummyPos lit)))
                    )
                    `shouldSatisfy` isTypeMismatch
                        [S.IntT]
        forM_ (createLiteralsForErrorTestExcept [P.FloatT]) $ \lit ->
            it ("rejects float vs " <> show (P.fromLiteral lit))
                $ snd
                    ( runCheck
                        globalEnv
                        (checkOperation dummyPos (P.Modulo (P.Literal dummyPos (P.Float 0.5)) (P.Literal dummyPos lit)))
                    )
                `shouldSatisfy` isTypeMismatch [S.FloatT]
  where
    isTypeMismatch expected [TypeMismatch e _] = e == expected
    isTypeMismatch _ _ = False
