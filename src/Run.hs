{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import Control.Monad.State (runState)
import Interpreter
import RIO
import SExp
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import Text.Megaparsec (errorBundlePretty, parse)
import Types

repl :: SymbolsTable -> InputT IO ()
repl state = do
    mInput <- getInputLine "% "
    let input = fromMaybe "" mInput
    let res = parse sexpParser "" input
    case res of
        Left eb -> do
            outputStrLn $ errorBundlePretty eb
            repl state
        Right sexp -> do
            outputStrLn $ show sexp
            let (evaluation, state') = runState (runEvaluator evaluator sexp) state
            case evaluation of
                Left ee -> outputStrLn $ show ee
                Right v -> outputStrLn $ show $ represent v
            repl state'

run :: RIO App ()
run = do
    env <- ask
    let options = appOptions env
    let suffix = if optionsVerbose options then " Verbose !!!" else ""
    logInfo $ "You are entering Mezima's REPL. " <> suffix
    let initState = [mempty]
    liftIO $ runInputT defaultSettings $ repl initState
