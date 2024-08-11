module Interpreter (EvaluationError (..), Evaluator, runEvaluator, evaluator, SymbolsTable, initSymbolsTable) where

import Control.Monad.State (State, evalState, get, put)
import RIO
import RIO.Map (insert, union)
import qualified RIO.Map
import SExp

type Evaluation = Either EvaluationError SExp
newtype Evaluator = Evaluator {runEvaluator :: SExp -> State SymbolsTable Evaluation}

evaluator :: Evaluator
evaluator = Evaluator{runEvaluator = runEvaluator_}

runEvaluator_ :: SExp -> State SymbolsTable Evaluation
runEvaluator_ sexp = do
    vEvaluation <- defaultEvaluate sexp
    return (represent <$> vEvaluation)

data Value
    = VBool Bool
    | VDouble Double
    | VInteger Integer
    | VString String
    | VList [Value]
    | VFunction [String] SExp (Map String Value)
    | VBuiltIn String ([VEvaluation] -> VEvaluation)

represent :: Value -> SExp
represent (VBool x) = SBool x
represent (VDouble x) = if x >= 0 then SDouble x else SSExp [minusIdentifier, SDouble (-x)]
represent (VInteger x) = if x >= 0 then SInteger x else SSExp [minusIdentifier, SInteger (-x)]
represent (VString x) = SString x
represent (VList xs) = SSExp (listIdentifier : fmap represent xs)
represent (VFunction params body _) = SSExp [functionIdentifier, SSExp paramsIds, body]
  where
    paramsIds = identifier <$> params
    identifier s = SId Identifier{id = s}
represent (VBuiltIn name _) = SString ("Built in function " <> name)

data EvaluationError
    = IdentifierError String
    | VTypeError
    | VArityError
    deriving (Show, Eq)

type VEvaluation = Either EvaluationError Value
type SymbolsTable = Map String Value

initSymbolsTable :: SymbolsTable
initSymbolsTable =
    RIO.Map.fromList
        [ ("=", VBuiltIn "=" evaluateEq)
        , ("+", VBuiltIn "+" evaluateSum)
        , ("*", VBuiltIn "*" evaluateProd)
        , ("-", VBuiltIn "-" evaluateNegative)
        , ("neg", VBuiltIn "neg" evaluateNegation)
        , ("and", VBuiltIn "and" evaluateAnd)
        , ("or", VBuiltIn "or" evaluateOr)
        , ("if", VBuiltIn "if" evaluateIfElse)
        , ("/", VBuiltIn "/" evaluateDiv)
        , ("list", VBuiltIn "list" evaluateList)
        ]

defaultEvaluate :: SExp -> State SymbolsTable VEvaluation
defaultEvaluate (SSExp (h : xs))
    | h == functionIdentifier =
        case xs of
            [identifiers, expression] -> do
                symbolsTable <- get
                return $ defineFunction symbolsTable identifiers expression
            _ -> return $ Left VArityError
defaultEvaluate (SSExp (h : xs))
    | h == defineIdentifier =
        case xs of
            [SId (Identifier{id = sid}), sexp] -> evaluateDefine sid sexp
            [_, _] -> return $ Left VTypeError
            _ -> return $ Left VArityError
defaultEvaluate (SSExp (h : xs))
    | h == defineFunctionIdentifier =
        case xs of
            [functionName, parameters, body] ->
                defaultEvaluate
                    ( SSExp
                        [ defineIdentifier
                        , functionName
                        , SSExp [functionIdentifier, parameters, body]
                        ]
                    )
            _ -> return $ Left VArityError
defaultEvaluate (SSExp (h : xs)) = do
    hValue <- defaultEvaluate h
    case hValue of
        Right (VFunction params body enclosingSymbols) -> do
            evaluations <- mapM defaultEvaluate xs
            case sequence evaluations of
                Left evaluationError -> return $ Left evaluationError
                Right values -> do
                    symbolsTable <- get
                    let argsSymbolsTable_ = argsSymbolsTable params values
                    let self = VFunction params body enclosingSymbols
                    let localSymbolsTable = insert "self" self argsSymbolsTable_
                    let state' = (localSymbolsTable `union` enclosingSymbols) `union` symbolsTable
                    let res = evalState (defaultEvaluate body) state'
                    return res
        Right (VBuiltIn _ f) -> do
            evaluations <- mapM defaultEvaluate xs
            return $ f evaluations
        Right _ -> return $ Left VTypeError
        Left ee -> return $ Left ee
defaultEvaluate (SSExp []) = return $ Left VTypeError
defaultEvaluate (SId (Identifier{id = sid})) = do
    symbolsTable <- get
    case RIO.Map.lookup sid symbolsTable of
        Just value -> return $ Right value
        Nothing -> return $ Left $ IdentifierError sid
defaultEvaluate (SBool x) = return $ Right (VBool x)
defaultEvaluate (SDouble x) = return $ Right (VDouble x)
defaultEvaluate (SInteger x) = return $ Right (VInteger x)
defaultEvaluate (SString x) = return $ Right (VString x)

evaluateSum :: [VEvaluation] -> VEvaluation
evaluateSum = foldr f (Right (VInteger 0))
  where
    f (Right v) _ | isNotNumeric v = Left VTypeError
    f (Left ee) _ = Left ee
    f _ (Left ee) = Left ee
    f (Right (VInteger a)) (Right (VInteger b)) = Right (VInteger (a + b))
    f (Right (VDouble a)) (Right (VDouble b)) = Right (VDouble (a + b))
    f (Right (VInteger a)) (Right (VDouble b)) = Right (VDouble (fromIntegral a + b))
    f (Right (VDouble a)) (Right (VInteger b)) = Right (VDouble (a + fromIntegral b))
    f _ _ = Left VTypeError

evaluateProd :: [VEvaluation] -> VEvaluation
evaluateProd = foldr f (Right (VInteger 1))
  where
    f (Right v) _ | isNotNumeric v = Left VTypeError
    f (Left ee) _ = Left ee
    f _ (Left ee) = Left ee
    f (Right (VInteger a)) (Right (VInteger b)) = Right (VInteger (a * b))
    f (Right (VDouble a)) (Right (VDouble b)) = Right (VDouble (a * b))
    f (Right (VInteger a)) (Right (VDouble b)) = Right (VDouble (fromIntegral a * b))
    f (Right (VDouble a)) (Right (VInteger b)) = Right (VDouble (a * fromIntegral b))
    f _ _ = Left VTypeError

evaluateEq :: [VEvaluation] -> VEvaluation
evaluateEq [] = Right $ VBool True
evaluateEq ((Right x) : _) | isNotComparable x = Left VTypeError
evaluateEq ((Right x) : rest) = foldr f (Right (VBool True)) rest
  where
    f (Left ee) _ = Left ee
    f (Right v) _ | isNotComparable v = Left VTypeError
    f (Right y) _ | different x y = Right (VBool False)
    f _ _ = Right (VBool True)
    different (VInteger i) (VInteger j) = i /= j
    different (VInteger i) (VDouble j) = fromIntegral i /= j
    different (VDouble i) (VInteger j) = i /= fromIntegral j
    different (VDouble i) (VDouble j) = i /= j
    different (VBool i) (VBool j) = i /= j
    different (VString i) (VString j) = i /= j
    different (VList i) (VList j) | length i /= length j = True
    different (VList i) (VList j) = any diff (zip i j)
    different _ _ = True
    diff (i, j) = different i j
evaluateEq ((Left ee) : _) = Left ee

evaluateNegative :: [VEvaluation] -> VEvaluation
evaluateNegative [Right (VInteger i)] = Right $ VInteger (-i)
evaluateNegative [Right (VDouble i)] = Right $ VDouble (-i)
evaluateNegative (Left ee : _) = Left ee
evaluateNegative [_] = Left VTypeError
evaluateNegative _ = Left VArityError

evaluateNegation :: [VEvaluation] -> VEvaluation
evaluateNegation [Right (VBool True)] = Right $ VBool False
evaluateNegation [Right (VBool False)] = Right $ VBool True
evaluateNegation (Left ee : _) = Left ee
evaluateNegation [_] = Left VTypeError
evaluateNegation _ = Left VArityError

evaluateDiv :: [VEvaluation] -> VEvaluation
evaluateDiv [Right (VInteger i)] = Right $ VDouble (1 / fromIntegral i)
evaluateDiv [Right (VDouble i)] = Right $ VDouble (1 / i)
evaluateDiv [Right (VInteger i), Right (VInteger j)] = Right $ VDouble (fromIntegral i / fromIntegral j)
evaluateDiv [Right (VDouble i), Right (VDouble j)] = Right $ VDouble (i / j)
evaluateDiv [Right (VDouble i), Right (VInteger j)] = Right $ VDouble (i / fromIntegral j)
evaluateDiv [Right (VInteger i), Right (VDouble j)] = Right $ VDouble (fromIntegral i / j)
evaluateDiv ((Left ee) : _) = Left ee
evaluateDiv ((Right x) : _) | isNotNumeric x = Left VTypeError
evaluateDiv (_ : (Left ee) : _) = Left ee
evaluateDiv (_ : (Right x) : _) | isNotNumeric x = Left VTypeError
evaluateDiv _ = Left VArityError

evaluateAnd :: [VEvaluation] -> VEvaluation
evaluateAnd = foldr f (Right (VBool True))
  where
    f (Right v) _ | isNotBoolean v = Left VTypeError
    f (Left ee) _ = Left ee
    f (Right (VBool False)) _ = Right (VBool False)
    f _ (Left ee) = Left ee
    f (Right (VBool True)) (Right (VBool v)) = Right (VBool v)
    f _ _ = Left VTypeError

evaluateOr :: [VEvaluation] -> VEvaluation
evaluateOr = foldr f (Right (VBool False))
  where
    f (Right v) _ | isNotBoolean v = Left VTypeError
    f (Left ee) _ = Left ee
    f (Right (VBool True)) _ = Right (VBool True)
    f _ (Left ee) = Left ee
    f (Right (VBool False)) (Right (VBool v)) = Right (VBool v)
    f _ _ = Left VTypeError

evaluateIfElse :: [VEvaluation] -> VEvaluation
evaluateIfElse [Right (VBool True), x, _] = x
evaluateIfElse [Right (VBool False), _, x] = x
evaluateIfElse ((Right c) : _) | isNotBoolean c = Left VTypeError
evaluateIfElse ((Left ee) : _) = Left ee
evaluateIfElse _ = Left VArityError

evaluateList :: [VEvaluation] -> VEvaluation
evaluateList evaluations = VList <$> sequence evaluations

evaluateDefine :: String -> SExp -> State SymbolsTable VEvaluation
evaluateDefine sid sexp = do
    evaluation <- defaultEvaluate sexp
    case evaluation of
        Right value -> do
            symbolsTable <- get
            let updatedSymbolsTable = insert sid value symbolsTable
            put updatedSymbolsTable
            return $ Right value
        Left ee -> return $ Left ee

defineFunction :: SymbolsTable -> SExp -> SExp -> VEvaluation
defineFunction symbolsTable (SSExp maybeIdentifiers) sexp = do
    identifiers <- mapM parseId maybeIdentifiers
    let enclosingSymbols = getEnclosingSymbols symbolsTable sexp
    Right (VFunction identifiers sexp enclosingSymbols)
defineFunction _ _ _ = Left VTypeError

argsSymbolsTable :: [String] -> [Value] -> Map String Value
argsSymbolsTable formalParams params = foldr f mempty (zip formalParams params)
  where
    f (s, v) = insert s v
parseId :: SExp -> Either EvaluationError String
parseId (SId (Identifier{id = identifier})) = Right identifier
parseId _ = Left VTypeError

getEnclosingSymbols :: SymbolsTable -> SExp -> Map String Value
getEnclosingSymbols symbolsTable (SSExp sexps) = foldr f mempty sexps
  where
    f sexp symbols = symbols `union` getEnclosingSymbols symbolsTable sexp
getEnclosingSymbols symbolsTable (SId (Identifier{id = id_})) =
    case RIO.Map.lookup id_ symbolsTable of
        Just v -> insert id_ v mempty
        Nothing -> mempty
getEnclosingSymbols _ _ = mempty

listIdentifier :: SExp
listIdentifier = SId (Identifier{id = "list"})

minusIdentifier :: SExp
minusIdentifier = SId (Identifier{id = "-"})

defineIdentifier :: SExp
defineIdentifier = SId (Identifier{id = "def"})

functionIdentifier :: SExp
functionIdentifier = SId (Identifier{id = "fn"})

defineFunctionIdentifier :: SExp
defineFunctionIdentifier = SId (Identifier{id = "defn"})

isNotNumeric :: Value -> Bool
isNotNumeric (VInteger _) = False
isNotNumeric (VDouble _) = False
isNotNumeric _ = True

isNotBoolean :: Value -> Bool
isNotBoolean (VBool _) = False
isNotBoolean _ = True

isNotComparable :: Value -> Bool
isNotComparable (VFunction{}) = True
isNotComparable (VBuiltIn{}) = True
isNotComparable (VList l) = any isNotComparable l
isNotComparable _ = False
