module InterpreterSpec (spec) where

import Control.Monad.State (evalState)
import Data.List (repeat)
import Interpreter (EvaluationError (..), Evaluator (runEvaluator), Value (..), evaluator, runEvaluator)
import RIO
import SExp
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    let evaluates sexp = evalState (runEvaluator evaluator sexp) [mempty]
    describe "evaluator basics" do
        let listIdentifier = SId (Identifier{id = "list"})
        prop "evaluates S Integer as S Integer" do
            \i -> evaluates (SInteger i) `shouldBe` Right (VInteger i)
        prop "evaluates S Double as S Double" do
            \i -> evaluates (SDouble i) `shouldBe` Right (VDouble i)
        prop "evaluates S String as S String" do
            \i -> evaluates (SString i) `shouldBe` Right (VString i)
        it "Evaluates list prefixed as lists" do
            evaluates (SSExp [listIdentifier]) `shouldBe` Right (VList [])
        it "Does not evaluate empty list" do
            evaluates (SSExp []) `shouldBe` Left NotImplementedYet
        it "Does not evaluate list with identifier" do
            evaluates (SSExp [SId (Identifier{id = "foo"})]) `shouldBe` Left NotImplementedYet
    describe "evaluator numerics" do
        let sumIdentifier = SId (Identifier{id = "+"})
        let equalityIdentifier = SId (Identifier{id = "="})
        let minusIdentifier = SId (Identifier{id = "-"})
        let divisionIdentifier = SId (Identifier{id = "/"})
        it "Evaluates empty sum as 0" do
            evaluates (SSExp [sumIdentifier]) `shouldBe` Right (VInteger 0)
        prop "should evaluate sum of integers as sum of integers" do
            \is -> evaluates (SSExp (sumIdentifier : fmap SInteger is)) `shouldBe` Right (VInteger (sum is))
        prop "Evaluates sum of doubles" do
            \ds -> isRight (evaluates (SSExp (sumIdentifier : fmap SDouble ds))) `shouldBe` True
        it "Returns type errors when summing non numerics" do
            evaluates (SSExp [sumIdentifier, SInteger 1, SString "foo"]) `shouldBe` Left VTypeError
        it "Evaluates sums lazily on type errors" do
            evaluates (SSExp (sumIdentifier : SInteger 5 : repeat (SString "foo"))) `shouldBe` Left VTypeError
        it "Evaluates nested sums" do
            evaluates (SSExp [sumIdentifier, SSExp [sumIdentifier, SInteger 1, SInteger 2], SInteger 3, SInteger 4]) `shouldBe` Right (VInteger 10)
        it "Evaluates equality without arguments as True" do
            evaluates (SSExp [equalityIdentifier]) `shouldBe` Right (VBool True)
        prop "Evaluates equality of similar integers as True" do
            \(i, n) -> evaluates (SSExp (equalityIdentifier : replicate i (SInteger n))) `shouldBe` Right (VBool True)
        prop "Evaluates equality of similar doubles as True" do
            \(i, d) -> evaluates (SSExp (equalityIdentifier : replicate i (SDouble d))) `shouldBe` Right (VBool True)
        it "Evaluates equal integers and and doubles as True" do
            evaluates (SSExp [equalityIdentifier, SDouble 1.0, SInteger 1]) `shouldBe` Right (VBool True)
        it "Evaluates equality of different integers as False" do
            evaluates (SSExp [equalityIdentifier, SInteger 1, SInteger 2]) `shouldBe` Right (VBool False)
        it "Evaluates equality of different doubles as False" do
            evaluates (SSExp [equalityIdentifier, SDouble 1.2, SDouble 2.4]) `shouldBe` Right (VBool False)
        it "Evaluates equality of non numeric as VTypeError" do
            evaluates (SSExp [equalityIdentifier, SString "foo"]) `shouldBe` Left VTypeError
        it "Lazily evaluates an equality returning VTypeError" do
            evaluates (SSExp (equalityIdentifier : SString "foo" : repeat (SInteger 42))) `shouldBe` Left VTypeError
        it "Lazily evaluates an equality returning False" do
            evaluates (SSExp (equalityIdentifier : SInteger 1 : SInteger 2 : repeat (SString "foo"))) `shouldBe` Right (VBool False)
        it "Evaluates negative integers" do
            evaluates (SSExp [minusIdentifier, SInteger 42]) `shouldBe` Right (VInteger (-42))
        it "Evaluates negative doubles" do
            evaluates (SSExp [minusIdentifier, SDouble 42.5]) `shouldBe` Right (VDouble (-42.5))
        it "Evaluates negative of an EvaluationError as an EvaluationError" do
            isLeft $ evaluates (SSExp [minusIdentifier, SSExp []])
        it "Evaluates negative of an empty list as an VArityError" do
            evaluates (SSExp [minusIdentifier]) `shouldBe` Left VArityError
        it "Evaluates negative of many integers an VArityError" do
            evaluates (SSExp [minusIdentifier, SInteger 42, SInteger 69]) `shouldBe` Left VArityError
        it "Evaluates negative of many doubles an VArityError" do
            evaluates (SSExp [minusIdentifier, SDouble 42.1, SDouble 69.0]) `shouldBe` Left VArityError
        it "Evaluates inversion of integers" do
            evaluates (SSExp [divisionIdentifier, SInteger 2]) `shouldBe` Right (VDouble 0.5)
        it "Evaluates inversion of doubles" do
            evaluates (SSExp [divisionIdentifier, SDouble 0.5]) `shouldBe` Right (VDouble 2.0)
        it "Evaluates division of integers" do
            evaluates (SSExp [divisionIdentifier, SInteger 2, SInteger 4]) `shouldBe` Right (VDouble 0.5)
        it "Evaluates division of doubles" do
            evaluates (SSExp [divisionIdentifier, SDouble 2.0, SDouble 0.5]) `shouldBe` Right (VDouble 4.0)
        it "Evaluates division of a double and an integer" do
            evaluates (SSExp [divisionIdentifier, SDouble 2.0, SInteger 1]) `shouldBe` Right (VDouble 2.0)
        it "Evaluates division of a integer and a double" do
            evaluates (SSExp [divisionIdentifier, SInteger 1, SDouble 0.5]) `shouldBe` Right (VDouble 2.0)
        it "Evaluates inversion of an EvaluationError as an EvaluationError" do
            isLeft $ evaluates (SSExp [divisionIdentifier, SSExp [SString "foo"]])
        it "Evaluates division of an EvaluationError as an EvaluationError" do
            isLeft $ evaluates (SSExp [divisionIdentifier, SInteger 2, SSExp [SString "foo"]])
        it "Evaluates inversion of an non numeric as VTypeError" do
            isLeft $ evaluates (SSExp [divisionIdentifier, SString "foo"])
        it "Evaluates inversion of an non numeric as VTypeError" do
            isLeft $ evaluates (SSExp [divisionIdentifier, SInteger 2, SString "foo"])
        it "Evaluates division of an EvaluationError and non numeric as EvaluationError" do
            let arityErrorExpression = SSExp [minusIdentifier, SInteger 2, SInteger 2, SInteger 2]
            evaluates
                ( SSExp
                    [ divisionIdentifier
                    , arityErrorExpression
                    , SString "foo"
                    ]
                )
                `shouldBe` Left VArityError
        it "Evaluates division of a non numeric and an EvaluationError as VTypeError" do
            let arityErrorExpression = SSExp [minusIdentifier, SInteger 2, SInteger 2, SInteger 2]
            evaluates
                ( SSExp
                    [ divisionIdentifier
                    , SString "foo"
                    , arityErrorExpression
                    ]
                )
                `shouldBe` Left VTypeError
        it "Evaluates a division oy more than two integers as an VArityError" do
            evaluates (SSExp [divisionIdentifier, SInteger 42, SInteger 3, SInteger 6]) `shouldBe` Left VArityError
    describe "logical operations" do
        let conjIdentifier = SId $ Identifier{id = "and"}
        let disjunctionIdent = SId $ Identifier{id = "or"}
        prop "Evaluates conjunction of booleans" do
            \bs ->
                evaluates
                    ( SSExp
                        (conjIdentifier : (SBool <$> bs))
                    )
                    `shouldBe` Right (VBool (and bs))
        it "Evaluates conjunction of non booleans as VTypeError" do
            evaluates (SSExp [conjIdentifier, SBool True, SString "foo"]) `shouldBe` Left VTypeError
        prop "Evaluates disjunction of booleans" do
            \bs ->
                evaluates
                    ( SSExp
                        (disjunctionIdent : (SBool <$> bs))
                    )
                    `shouldBe` Right (VBool (or bs))
        it "Evaluates conjunction of non booleans as VTypeError" do
            evaluates (SSExp [disjunctionIdent, SBool False, SString "foo"]) `shouldBe` Left VTypeError
