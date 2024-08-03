-- {-# LANGUAGE BangPatterns #-}

module Interpreter (EvaluationError (..), Evaluator, runEvaluator, evaluator, represent, Value (..), SymbolsTable) where

import Control.Monad.State (State, evalState, get, put)
import RIO
import qualified RIO.List
import RIO.Map (insert)
import qualified RIO.Map
import SExp

-- import GHC.IO (unsafePerformIO)
-- import qualified Prelude

data Value
    = VBool Bool
    | VDouble Double
    | VInteger Integer
    | VString String
    | VList [Value]
    | VFunction [String] SExp
    deriving (Eq, Show)

represent :: Value -> SExp
represent (VBool x) = SBool x
represent (VDouble x) = if x >= 0 then SDouble x else SSExp [_minusIdentifier, SDouble (-x)]
represent (VInteger x) = if x >= 0 then SInteger x else SSExp [_minusIdentifier, SInteger (-x)]
represent (VString x) = SString x
represent (VList xs) = SSExp (_listIdentifier : fmap represent xs)
represent (VFunction params body) = SSExp [_functionIdentifier, SSExp paramsIds, body]
  where
    paramsIds = identifier <$> params
    identifier s = SId Identifier{id = s}

data EvaluationError
    = NotImplementedYet
    | VTypeError
    | VArityError
    deriving (Show, Eq)

type Evaluation = Either EvaluationError Value
type SymbolsTable = [Map String Value]

newtype Evaluator = Evaluator {runEvaluator :: SExp -> State SymbolsTable Evaluation}

evaluator :: Evaluator
evaluator = Evaluator{runEvaluator = _defaultEvaluate}

_defaultEvaluate :: SExp -> State SymbolsTable Evaluation
_defaultEvaluate (SSExp (h : xs)) | h == _listIdentifier = do
    evaluations <- mapM _defaultEvaluate xs
    return (VList <$> sequence evaluations)
_defaultEvaluate (SSExp (h : xs))
    | h == _sumIdentifier = _evaluateArgs xs _evaluateSum
_defaultEvaluate (SSExp (h : xs))
    | h == _prodIdentifier = _evaluateArgs xs _evaluateProd
_defaultEvaluate (SSExp (h : xs))
    | h == _minusIdentifier = _evaluateArgs xs _evaluateNegative
_defaultEvaluate (SSExp (h : xs))
    | h == _equalityIdentifier = _evaluateArgs xs _evaluateEq
_defaultEvaluate (SSExp (h : xs))
    | h == _divisionIdentifier = _evaluateArgs xs _evaluateDiv
_defaultEvaluate (SSExp (h : xs))
    | h == _conjIdentifer = _evaluateArgs xs _evaluateAnd
_defaultEvaluate (SSExp (h : xs))
    | h == _disjunctionIdentifier = _evaluateArgs xs _evaluateOr
_defaultEvaluate (SSExp (h : xs))
    | h == _negationIdentifier = _evaluateArgs xs _evaluateNegation
_defaultEvaluate (SSExp (h : xs))
    | h == _ifIdentifier = _evaluateArgs xs _evaluateIfElse
_defaultEvaluate (SSExp (h : xs))
    | h == _functionIdentifier =
        case xs of
            [identifiers, expression] -> return $ _defineFunction identifiers expression
            _ -> return $ Left VArityError
_defaultEvaluate (SSExp (h : xs))
    | h == _defineIdentifier =
        case xs of
            [SId (Identifier{id = sid}), sexp] -> _evaluateDefine sid sexp
            [_, _] -> return $ Left VTypeError
            _ -> return $ Left VArityError
_defaultEvaluate (SSExp (h : xs))
    | h == _defineFunctionIdentifier =
        case xs of
            [functionName, parameters, body] ->
                _defaultEvaluate
                    ( SSExp
                        [ _defineIdentifier
                        , functionName
                        , SSExp [_functionIdentifier, parameters, body]
                        ]
                    )
            _ -> return $ Left VArityError
_defaultEvaluate (SSExp (h : xs)) = do
    hValue <- _defaultEvaluate h
    case hValue of
        Right (VFunction params body) -> do
            {-
            let !_ = unsafePerformIO (Prelude.print "params")
            let !_ = unsafePerformIO (Prelude.print params)
            let !_ = unsafePerformIO (Prelude.print "body")
            let !_ = unsafePerformIO $ Prelude.print body
            let !_ = unsafePerformIO $ Prelude.print "xs"
            let !_ = unsafePerformIO $ Prelude.print xs
            -}
            evaluations <- mapM _defaultEvaluate xs
            symbolsTable <- get
            {-
            let !_ = unsafePerformIO $ Prelude.print "evaluations"
            let !_ = unsafePerformIO $ Prelude.print evaluations
            -}
            let upToDateSymbolsTable = _updateSymbolsTable params evaluations
            {-
            let !_ = unsafePerformIO (Prelude.print "state")
            let !_ = unsafePerformIO $ Prelude.print (upToDateSymbolsTable : symbolsTable)
            let !_ = unsafePerformIO Prelude.getLine
            -}
            let res = evalState (_defaultEvaluate body) (upToDateSymbolsTable : symbolsTable)
            {-
            let !_ = unsafePerformIO (Prelude.print "res")
            let !_ = unsafePerformIO (Prelude.print res)
            -}
            return res
        Right _ -> return $ Left VTypeError
        Left ee -> return $ Left ee
_defaultEvaluate (SSExp _) = return $ Left NotImplementedYet
_defaultEvaluate (SId (Identifier{id = sid})) = do
    symbolsTable <- get
    case _lookupSt sid symbolsTable of
        Just value -> return $ Right value
        Nothing -> return $ Left NotImplementedYet
_defaultEvaluate (SBool x) = return $ Right (VBool x)
_defaultEvaluate (SDouble x) = return $ Right (VDouble x)
_defaultEvaluate (SInteger x) = return $ Right (VInteger x)
_defaultEvaluate (SString x) = return $ Right (VString x)

_evaluateArgs :: [SExp] -> ([Evaluation] -> Evaluation) -> State SymbolsTable Evaluation
_evaluateArgs args reduce = do
    evaluations <- mapM _defaultEvaluate args
    return $ reduce evaluations

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

_evaluateNegative :: [Evaluation] -> Evaluation
_evaluateNegative [Right (VInteger i)] = Right $ VInteger (-i)
_evaluateNegative [Right (VDouble i)] = Right $ VDouble (-i)
_evaluateNegative (Left ee : _) = Left ee
_evaluateNegative [_] = Left VTypeError
_evaluateNegative _ = Left VArityError

_evaluateNegation :: [Evaluation] -> Evaluation
_evaluateNegation [Right (VBool True)] = Right $ VBool False
_evaluateNegation [Right (VBool False)] = Right $ VBool True
_evaluateNegation (Left ee : _) = Left ee
_evaluateNegation [_] = Left VTypeError
_evaluateNegation _ = Left VArityError

_evaluateDiv :: [Evaluation] -> Evaluation
_evaluateDiv [Right (VInteger i)] = Right $ VDouble (1 / fromIntegral i)
_evaluateDiv [Right (VDouble i)] = Right $ VDouble (1 / i)
_evaluateDiv [Right (VInteger i), Right (VInteger j)] = Right $ VDouble (fromIntegral i / fromIntegral j)
_evaluateDiv [Right (VDouble i), Right (VDouble j)] = Right $ VDouble (i / j)
_evaluateDiv [Right (VDouble i), Right (VInteger j)] = Right $ VDouble (i / fromIntegral j)
_evaluateDiv [Right (VInteger i), Right (VDouble j)] = Right $ VDouble (fromIntegral i / j)
_evaluateDiv ((Left ee) : _) = Left ee
_evaluateDiv ((Right x) : _) | _isNotNumeric x = Left VTypeError
_evaluateDiv (_ : (Left ee) : _) = Left ee
_evaluateDiv (_ : (Right x) : _) | _isNotNumeric x = Left VTypeError
_evaluateDiv _ = Left VArityError

_evaluateAnd :: [Evaluation] -> Evaluation
_evaluateAnd = foldr f (Right (VBool True))
  where
    f (Right v) _ | _isNotBoolean v = Left VTypeError
    f (Left ee) _ = Left ee
    f (Right (VBool False)) _ = Right (VBool False)
    f _ (Left ee) = Left ee
    f (Right (VBool True)) (Right (VBool v)) = Right (VBool v)
    f _ _ = Left VTypeError

_evaluateOr :: [Evaluation] -> Evaluation
_evaluateOr = foldr f (Right (VBool False))
  where
    f (Right v) _ | _isNotBoolean v = Left VTypeError
    f (Left ee) _ = Left ee
    f (Right (VBool True)) _ = Right (VBool True)
    f _ (Left ee) = Left ee
    f (Right (VBool False)) (Right (VBool v)) = Right (VBool v)
    f _ _ = Left VTypeError

_evaluateIfElse :: [Evaluation] -> Evaluation
_evaluateIfElse [Right (VBool True), x, _] = x
_evaluateIfElse [Right (VBool False), _, x] = x
_evaluateIfElse ((Right c) : _) | _isNotBoolean c = Left VTypeError
_evaluateIfElse ((Left ee) : _) = Left ee
_evaluateIfElse _ = Left VArityError

_evaluateDefine :: String -> SExp -> State SymbolsTable Evaluation
_evaluateDefine sid sexp = do
    evaluation <- _defaultEvaluate sexp
    case evaluation of
        Right value -> do
            symbolsTable <- get
            let st0 = fromMaybe mempty $ RIO.List.headMaybe symbolsTable
            let sts = fromMaybe [] $ RIO.List.tailMaybe symbolsTable
            let updatedSymbolsTable = insert sid value st0
            put (updatedSymbolsTable : sts)
            return $ Right value
        Left ee -> return $ Left ee

_defineFunction :: SExp -> SExp -> Evaluation
_defineFunction (SSExp maybeIdentifiers) sexp = do
    identifiers <- mapM _parseId maybeIdentifiers
    Right (VFunction identifiers sexp)
_defineFunction _ _ = Left VTypeError

_updateSymbolsTable :: [String] -> [Evaluation] -> Map String Value
_updateSymbolsTable formalParams params = foldr f mempty (zip formalParams params)
  where
    f (_, Left _) st_ = st_
    f (s, Right v) st_ = insert s v st_
_parseId :: SExp -> Either EvaluationError String
_parseId (SId (Identifier{id = identifier})) = Right identifier
_parseId _ = Left VTypeError

_lookupSt :: String -> SymbolsTable -> Maybe Value
_lookupSt _ [] = Nothing
_lookupSt k (h : rest) = case RIO.Map.lookup k h of
    Nothing -> _lookupSt k rest
    Just x -> Just x
_listIdentifier :: SExp
_listIdentifier = SId (Identifier{id = "list"})

_sumIdentifier :: SExp
_sumIdentifier = SId (Identifier{id = "+"})

_prodIdentifier :: SExp
_prodIdentifier = SId (Identifier{id = "*"})

_equalityIdentifier :: SExp
_equalityIdentifier = SId (Identifier{id = "="})

_minusIdentifier :: SExp
_minusIdentifier = SId (Identifier{id = "-"})

_divisionIdentifier :: SExp
_divisionIdentifier = SId (Identifier{id = "/"})

_conjIdentifer :: SExp
_conjIdentifer = SId (Identifier{id = "and"})

_disjunctionIdentifier :: SExp
_disjunctionIdentifier = SId (Identifier{id = "or"})

_negationIdentifier :: SExp
_negationIdentifier = SId (Identifier{id = "neg"})

_ifIdentifier :: SExp
_ifIdentifier = SId (Identifier{id = "if"})

_defineIdentifier :: SExp
_defineIdentifier = SId (Identifier{id = "def"})

_functionIdentifier :: SExp
_functionIdentifier = SId (Identifier{id = "fn"})

_defineFunctionIdentifier :: SExp
_defineFunctionIdentifier = SId (Identifier{id = "defn"})

_isNotNumeric :: Value -> Bool
_isNotNumeric (VInteger _) = False
_isNotNumeric (VDouble _) = False
_isNotNumeric _ = True
_isNotBoolean :: Value -> Bool
_isNotBoolean (VBool _) = False
_isNotBoolean _ = True
