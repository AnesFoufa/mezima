module InterpreterSpec (spec) where

import Interpreter (EvaluationError (..), evaluator, runEvaluator)
import RIO
import SExp
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "evaluator" do
        prop "evaluates S Integer as S Integer" do
            \i -> runEvaluator evaluator (SInteger i) `shouldBe` Right (SInteger i)
        prop "evaluates S Double as S Double" do
            \i -> runEvaluator evaluator (SDouble i) `shouldBe` Right (SDouble i)
        prop "evaluates S String as S String" do
            \i -> runEvaluator evaluator (SString i) `shouldBe` Right (SString i)
        prop "Does not evaluate S Identifier" do
            \i -> runEvaluator evaluator (SId (Identifier{id = i})) `shouldBe` Left NotImplementedYet
        prop "Does not evaluate S Lists" do
            \i -> runEvaluator evaluator (SSExp i) `shouldBe` Left NotImplementedYet
