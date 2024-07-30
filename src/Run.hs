{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import Control.Monad.State (runState)
import Interpreter
import RIO
import SExp
import Text.Megaparsec (errorBundlePretty, parse)
import Types
import Prelude (getLine)

run :: RIO App ()
run = do
    env <- ask
    let options = appOptions env
    let suffix = if optionsVerbose options then " Verbose !!!" else ""
    logInfo $ "We're inside the application!" <> suffix
    let initState = [mempty]
    repl initState
  where
    repl state = do
        line <- liftIO getLine
        let res = parse sexpParser "" line
        case res of
            Left be -> do
                logInfo $ fromString $ errorBundlePretty be
                repl state
            Right sexp -> do
                logInfo $ fromString $ show sexp
                let (result, nState) = runState (runEvaluator evaluator sexp) state
                case result of
                    Left ee -> logInfo $ fromString $ show ee
                    Right v -> logInfo $ fromString $ show $ represent v
                repl nState
