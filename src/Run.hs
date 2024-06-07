{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import Import

run :: RIO App ()
run = do
    env <- ask
    let options = appOptions env
    let suffix = if optionsVerbose options then " Verbose !!!" else ""
    logInfo $ "We're inside the application!" <> suffix
