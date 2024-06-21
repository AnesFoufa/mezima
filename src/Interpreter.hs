module Interpreter (EvaluationError (..), Evaluator, runEvaluator, evaluator, represent, Value (..)) where

import RIO
import SExp

data Value
    = VBool Bool
    | VDouble Double
    | VInteger Integer
    | VString String
    | VList [Value]
    deriving (Eq, Show)

represent :: Value -> SExp
represent (VBool x) = SBool x
represent (VDouble x) = SDouble x
represent (VInteger x) = SInteger x
represent (VString x) = SString x
represent (VList xs) = SSExp (_listIdentifier : fmap represent xs)

data EvaluationError
    = NotImplementedYet
    | VTypeError
    deriving (Show, Eq)

newtype Evaluator = Evaluator {runEvaluator :: SExp -> Either EvaluationError Value}

evaluator :: Evaluator
evaluator = Evaluator{runEvaluator = _defaultEvaluate}

_defaultEvaluate :: SExp -> Either EvaluationError Value
_defaultEvaluate (SSExp (h : xs))
    | h == _listIdentifier = VList <$> mapM _defaultEvaluate xs
_defaultEvaluate (SSExp (h : xs))
    | h == _sumIdentifier = _evaluateSum $ fmap _defaultEvaluate xs
_defaultEvaluate (SSExp _) = Left NotImplementedYet
_defaultEvaluate (SId _) = Left NotImplementedYet
_defaultEvaluate (SBool x) = Right (VBool x)
_defaultEvaluate (SDouble x) = Right (VDouble x)
_defaultEvaluate (SInteger x) = Right (VInteger x)
_defaultEvaluate (SString x) = Right (VString x)

_evaluateSum :: [Either EvaluationError Value] -> Either EvaluationError Value
_evaluateSum = foldr f (Right (VInteger 0))
  where
    f (Right v) _ | isNotNumeric v = Left VTypeError
    f (Left ee) _ = Left ee
    f _ (Left ee) = Left ee
    f (Right (VInteger a)) (Right (VInteger b)) = Right (VInteger (a + b))
    f (Right (VDouble a)) (Right (VDouble b)) = Right (VDouble (a + b))
    f (Right (VInteger a)) (Right (VDouble b)) = Right (VDouble (fromIntegral a + b))
    f (Right (VDouble a)) (Right (VInteger b)) = Right (VDouble (a + fromIntegral b))
    f _ _ = Left VTypeError
    isNotNumeric (VInteger _) = False
    isNotNumeric (VDouble _) = False
    isNotNumeric _ = True

_listIdentifier :: SExp
_listIdentifier = SId (Identifier{id = "list"})

_sumIdentifier :: SExp
_sumIdentifier = SId (Identifier{id = "+"})
