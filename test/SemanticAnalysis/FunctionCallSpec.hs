module SemanticAnalysis.FunctionCallSpec (spec) where

import SemanticAnalysis (checkFunctionCall)
import qualified SemanticAnalysisTypes as S (ElaboratedType (..))
import SemanticAnalysisTypes (TypedExpression (..), SemanticError (..), Env(..))
import SpecUtils (mkEnv, intLit, boolLit, runCheck, ident)
import Test.Hspec
import qualified Data.Text as T

spec :: Spec
spec = xdescribe "checkFunctionCall" $ do
    it "resolves a valid call with matching arg types" $ do
        let env = mkEnv [(T.pack "add", ([S.IntT, S.IntT], S.IntT))]
            (typedExpr, typedArgs) =
                fst $ runCheck env (checkFunctionCall (ident $ T.pack "add") [intLit 1, intLit 2])

        texprType typedExpr `shouldBe` S.IntT
        map texprType typedArgs `shouldBe` [S.IntT, S.IntT]

    it "reports UseOfUndeclaredIdentifier for an unknown function" $ do
        let env = mkEnv []
            errs =
                snd $ runCheck env (checkFunctionCall (ident $ T.pack "missing") [])

        errs `shouldBe` [UseOfUndeclaredIdentifier (T.pack "missing")]

    it "reports InvalidArguments on an arg type mismatch" $ do
        let env = mkEnv [(T.pack "add", ([S.IntT, S.IntT], S.IntT))]
            errs =
                snd $ runCheck env (checkFunctionCall (ident $ T.pack "add") [intLit 1, boolLit True])

        errs `shouldBe` [InvalidArguments (T.pack "add") [S.IntT, S.IntT] [S.IntT, S.BoolT]]

    it "reports InvalidArguments on an arg count mismatch" $ do
        let env = mkEnv [(T.pack "add", ([S.IntT, S.IntT], S.IntT))]
            errs =
                snd $ runCheck env (checkFunctionCall (ident $ T.pack "add") [intLit 1])

        errs `shouldBe` [InvalidArguments (T.pack "add") [S.IntT, S.IntT] [S.IntT]]

    it "handles a zero-argument call" $ do
        let env = mkEnv [(T.pack "noop", ([], S.BoolT))]
            (typedExpr, typedArgs) =
                fst $ runCheck env (checkFunctionCall (ident $ T.pack "noop") [])

        typedArgs `shouldBe` []
        texprType typedExpr `shouldBe` S.BoolT

    it "looks up functions through parent scopes" $ do
        let parentEnv = mkEnv [(T.pack "add", ([S.IntT, S.IntT], S.IntT))]
            env = (mkEnv []){parent = Just parentEnv}
            (typedExpr, _) =
                fst $ runCheck env (checkFunctionCall (ident $ T.pack "add") [intLit 1, intLit 2])

        texprType typedExpr `shouldBe` S.IntT
