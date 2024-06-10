module Interpreter (EvaluationError (..), Evaluator, runEvaluator, evaluator) where

import RIO
import SExp

data EvaluationError = NotImplementedYet deriving (Show, Eq)
newtype Evaluator = Evaluator {runEvaluator :: SExp -> Either EvaluationError SExp}

evaluator :: Evaluator
evaluator = Evaluator{runEvaluator = _defaultEvaluate}

_defaultEvaluate :: SExp -> Either EvaluationError SExp
_defaultEvaluate (SSExp _) = Left NotImplementedYet
_defaultEvaluate (SId _) = Left NotImplementedYet
_defaultEvaluate sexp = Right sexp
