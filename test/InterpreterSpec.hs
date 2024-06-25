module InterpreterSpec (spec) where

import Data.List (repeat)
import Interpreter (EvaluationError (..), Value (..), evaluator, runEvaluator)
import RIO
import SExp
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "evaluator basics" do
        let listIdentifier = SId (Identifier{id = "list"})
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
    describe "evaluator numerics" do
        let sumIdentifier = SId (Identifier{id = "+"})
        let equalityIdentifier = SId (Identifier{id = "="})
        let minusIdentifier = SId (Identifier{id = "-"})
        it "Evaluates empty sum as 0" do
            runEvaluator evaluator (SSExp [sumIdentifier]) `shouldBe` Right (VInteger 0)
        prop "should evaluate sum of integers as sum of integers" do
            \is -> runEvaluator evaluator (SSExp (sumIdentifier : fmap SInteger is)) `shouldBe` Right (VInteger (sum is))
        prop "Evaluates sum of doubles" do
            \ds -> isRight (runEvaluator evaluator (SSExp (sumIdentifier : fmap SDouble ds))) `shouldBe` True
        it "Returns type errors when summing non numerics" do
            runEvaluator evaluator (SSExp [sumIdentifier, SInteger 1, SString "foo"]) `shouldBe` Left VTypeError
        it "Evaluates sums lazily on type errors" do
            runEvaluator evaluator (SSExp (sumIdentifier : SInteger 5 : repeat (SString "foo"))) `shouldBe` Left VTypeError
        it "Evaluates nested sums" do
            runEvaluator evaluator (SSExp [sumIdentifier, SSExp [sumIdentifier, SInteger 1, SInteger 2], SInteger 3, SInteger 4]) `shouldBe` Right (VInteger 10)
        it "Evaluates equality without arguments as True" do
            runEvaluator evaluator (SSExp [equalityIdentifier]) `shouldBe` Right (VBool True)
        prop "Evaluates equality of similar integers as True" do
            \(i, n) -> runEvaluator evaluator (SSExp (equalityIdentifier : replicate i (SInteger n))) `shouldBe` Right (VBool True)
        prop "Evaluates equality of similar doubles as True" do
            \(i, d) -> runEvaluator evaluator (SSExp (equalityIdentifier : replicate i (SDouble d))) `shouldBe` Right (VBool True)
        it "Evaluates equal integers and and doubles as True" do
            runEvaluator evaluator (SSExp [equalityIdentifier, SDouble 1.0, SInteger 1]) `shouldBe` Right (VBool True)
        it "Evaluates equality of different integers as False" do
            runEvaluator evaluator (SSExp [equalityIdentifier, SInteger 1, SInteger 2]) `shouldBe` Right (VBool False)
        it "Evaluates equality of different doubles as False" do
            runEvaluator evaluator (SSExp [equalityIdentifier, SDouble 1.2, SDouble 2.4]) `shouldBe` Right (VBool False)
        it "Evaluates equality of non numeric as VTypeError" do
            runEvaluator evaluator (SSExp [equalityIdentifier, SString "foo"]) `shouldBe` Left VTypeError
        it "Lazily evaluates an equality returning VTypeError" do
            runEvaluator evaluator (SSExp (equalityIdentifier : SString "foo" : repeat (SInteger 42))) `shouldBe` Left VTypeError
        it "Lazily evaluates an equality returning False" do
            runEvaluator evaluator (SSExp (equalityIdentifier : SInteger 1 : SInteger 2 : repeat (SString "foo"))) `shouldBe` Right (VBool False)
        it "Evaluates negative integers" do
            runEvaluator evaluator (SSExp [minusIdentifier, SInteger 42]) `shouldBe` Right (VInteger (-42))
        it "Evaluates negative doubles" do
            runEvaluator evaluator (SSExp [minusIdentifier, SDouble 42.5]) `shouldBe` Right (VDouble (-42.5))
        it "Evaluates negative of an EvaluationError as an EvaluationError" do
            isLeft $ runEvaluator evaluator (SSExp [minusIdentifier, SSExp []])
        it "Evaluates negative of an empty list as an VArityError" do
            runEvaluator evaluator (SSExp [minusIdentifier]) `shouldBe` Left VArityError
        it "Evaluates negative of many integers an VArityError" do
            runEvaluator evaluator (SSExp [minusIdentifier, SInteger 42, SInteger 69]) `shouldBe` Left VArityError
        it "Evaluates negative of many doubles an VArityError" do
            runEvaluator evaluator (SSExp [minusIdentifier, SDouble 42.1, SDouble 69.0]) `shouldBe` Left VArityError
