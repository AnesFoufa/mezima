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

type Evaluation = Either EvaluationError Value

newtype Evaluator = Evaluator {runEvaluator :: SExp -> Evaluation}

evaluator :: Evaluator
evaluator = Evaluator{runEvaluator = _defaultEvaluate}

_defaultEvaluate :: SExp -> Evaluation
_defaultEvaluate (SSExp (h : xs))
    | h == _listIdentifier = VList <$> mapM _defaultEvaluate xs
_defaultEvaluate (SSExp (h : xs))
    | h == _sumIdentifier = _evaluateSum $ fmap _defaultEvaluate xs
_defaultEvaluate (SSExp (h : xs))
    | h == _prodIdentifier = _evaluateProd $ fmap _defaultEvaluate xs
_defaultEvaluate (SSExp (h : xs))
    | h == _equalityIdentifier = _evaluateEq $ fmap _defaultEvaluate xs
_defaultEvaluate (SSExp _) = Left NotImplementedYet
_defaultEvaluate (SId _) = Left NotImplementedYet
_defaultEvaluate (SBool x) = Right (VBool x)
_defaultEvaluate (SDouble x) = Right (VDouble x)
_defaultEvaluate (SInteger x) = Right (VInteger x)
_defaultEvaluate (SString x) = Right (VString x)

_evaluateSum :: [Evaluation] -> Evaluation
_evaluateSum = foldr f (Right (VInteger 0))
  where
    f (Right v) _ | _isNotNumeric v = Left VTypeError
    f (Left ee) _ = Left ee
    f _ (Left ee) = Left ee
    f (Right (VInteger a)) (Right (VInteger b)) = Right (VInteger (a + b))
    f (Right (VDouble a)) (Right (VDouble b)) = Right (VDouble (a + b))
    f (Right (VInteger a)) (Right (VDouble b)) = Right (VDouble (fromIntegral a + b))
    f (Right (VDouble a)) (Right (VInteger b)) = Right (VDouble (a + fromIntegral b))
    f _ _ = Left VTypeError

_evaluateProd :: [Evaluation] -> Evaluation
_evaluateProd = foldr f (Right (VInteger 1))
  where
    f (Right v) _ | _isNotNumeric v = Left VTypeError
    f (Left ee) _ = Left ee
    f _ (Left ee) = Left ee
    f (Right (VInteger a)) (Right (VInteger b)) = Right (VInteger (a * b))
    f (Right (VDouble a)) (Right (VDouble b)) = Right (VDouble (a * b))
    f (Right (VInteger a)) (Right (VDouble b)) = Right (VDouble (fromIntegral a * b))
    f (Right (VDouble a)) (Right (VInteger b)) = Right (VDouble (a * fromIntegral b))
    f _ _ = Left VTypeError

_evaluateEq :: [Evaluation] -> Evaluation
_evaluateEq [] = Right $ VBool True
_evaluateEq ((Right x) : _) | _isNotNumeric x = Left VTypeError
_evaluateEq ((Right x) : rest) = foldr f (Right (VBool True)) rest
  where
    f (Left ee) _ = Left ee
    f (Right y) _ | _isNotNumeric y = Left VTypeError
    f (Right y) _ | numericDifferent x y = Right (VBool False)
    f _ _ = Right (VBool True)
    numericDifferent (VInteger i) (VInteger j) = i /= j
    numericDifferent (VInteger i) (VDouble j) = fromIntegral i /= j
    numericDifferent (VDouble i) (VInteger j) = i /= fromIntegral j
    numericDifferent (VDouble i) (VDouble j) = i /= j
    numericDifferent _ _ = False
_evaluateEq ((Left ee) : _) = Left ee

_listIdentifier :: SExp
_listIdentifier = SId (Identifier{id = "list"})

_sumIdentifier :: SExp
_sumIdentifier = SId (Identifier{id = "+"})

_prodIdentifier :: SExp
_prodIdentifier = SId (Identifier{id = "*"})

_equalityIdentifier :: SExp
_equalityIdentifier = SId (Identifier{id = "="})

_isNotNumeric :: Value -> Bool
_isNotNumeric (VInteger _) = False
_isNotNumeric (VDouble _) = False
_isNotNumeric _ = True
