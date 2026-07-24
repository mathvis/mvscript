module SemanticAnalysisSpec (spec) where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriter)
import qualified Data.Text as T
import qualified ParserTypes as P (Expression (..), Literal (..))
import SemanticAnalysis (checkExpression, checkLiteral)
import SemanticAnalysisTypes (Check, Env, SemanticError (..), TypedExpression (..), globalEnv)
import qualified SemanticAnalysisTypes as S (ElaboratedType (..), ResolvedLiteral (..))
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
      runCheck globalEnv (checkLiteral dummyPos (P.String (T.pack "test"))) `shouldBe` (S.String (T.pack "test"), [])
    it "resolves a float literal" $
      runCheck globalEnv (checkLiteral dummyPos (P.Float 5.4)) `shouldBe` (S.Float 5.4, [])
    it "resolves a bool literal" $
      runCheck globalEnv (checkLiteral dummyPos (P.Bool True)) `shouldBe` (S.Bool True, [])
  describe "checkExpression typing" $ do
    it "types an int literal correctly" $
      runCheck globalEnv (checkExpression (P.Literal dummyPos (P.Int 5))) `shouldSatisfy` exprIsTyped S.IntT
    it "types a string literal correctly" $
      runCheck globalEnv (checkExpression (P.Literal dummyPos (P.String (T.pack "test")))) `shouldSatisfy` exprIsTyped S.StringT
    it "types a float literal correctly" $
      runCheck globalEnv (checkExpression (P.Literal dummyPos (P.Float 5.4))) `shouldSatisfy` exprIsTyped S.FloatT
    it "types a bool literal correctly" $
      runCheck globalEnv (checkExpression (P.Literal dummyPos (P.Bool True))) `shouldSatisfy` exprIsTyped S.BoolT
  where
    exprIsTyped typ (expr, _) = texprType expr == typ
