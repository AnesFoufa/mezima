module InterpreterSpec (spec) where

import Data.List (repeat)
import Interpreter (EvaluationError (..), Value (..), evaluator, runEvaluator)
import RIO
import SExp
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "evaluator" do
        let listIdentifier = SId (Identifier{id = "list"})
        let sumIndentifier = SId (Identifier{id = "+"})
        prop "evaluates S Integer as S Integer" do
            \i -> runEvaluator evaluator (SInteger i) `shouldBe` Right (VInteger i)
        prop "evaluates S Double as S Double" do
            \i -> runEvaluator evaluator (SDouble i) `shouldBe` Right (VDouble i)
        prop "evaluates S String as S String" do
            \i -> runEvaluator evaluator (SString i) `shouldBe` Right (VString i)
        prop "Does not evaluate S Identifier" do
            \i -> runEvaluator evaluator (SId (Identifier{id = i})) `shouldBe` Left NotImplementedYet
        it "Evaluates list prefixed as lists" do
            runEvaluator evaluator (SSExp [listIdentifier]) `shouldBe` Right (VList [])
        prop "Does not evaluate S Lists" do
            \i -> runEvaluator evaluator (SSExp i) `shouldBe` Left NotImplementedYet
        it "Does not evaluate empty list" do
            runEvaluator evaluator (SSExp []) `shouldBe` Left NotImplementedYet
        it "Does not evaluate list with identifier" do
            runEvaluator evaluator (SSExp [SId (Identifier{id = "foo"})]) `shouldBe` Left NotImplementedYet
        it "Evaluates empty sum as 0" do
            runEvaluator evaluator (SSExp [sumIndentifier]) `shouldBe` Right (VInteger 0)
        prop "should evaluate sum of integers as sum of integers" do
            \is -> runEvaluator evaluator (SSExp (sumIndentifier : fmap SInteger is)) `shouldBe` Right (VInteger (sum is))
        prop "Evaluates sum of doubles" do
            \ds -> isRight (runEvaluator evaluator (SSExp (sumIndentifier : fmap SDouble ds))) `shouldBe` True
        it "Returns type errors when summing non numerics" do
            runEvaluator evaluator (SSExp [sumIndentifier, SInteger 1, SString "foo"]) `shouldBe` Left VTypeError
        it "Evaluates sums lazily on type errors" do
            runEvaluator evaluator (SSExp (sumIndentifier : SInteger 5 : repeat (SString "foo"))) `shouldBe` Left VTypeError
        it "Evaluates nested sums" do
            runEvaluator evaluator (SSExp [sumIndentifier, SSExp [sumIndentifier, SInteger 1, SInteger 2], SInteger 3, SInteger 4]) `shouldBe` Right (VInteger 10)
