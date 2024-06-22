{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

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
    repl
  where
    repl = do
        line <- liftIO getLine
        let res = parse sexpParser "" line
        case res of
            Left be -> logInfo $ fromString $ errorBundlePretty be
            Right sexp -> do
                logInfo $ fromString $ show sexp
                case runEvaluator evaluator sexp of
                    Left ee -> logInfo $ fromString $ show ee
                    Right v -> logInfo $ fromString $ show $ represent v
        repl
