{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import RIO
import Types

run :: RIO App ()
run = do
    env <- ask
    let options = appOptions env
    let suffix = if optionsVerbose options then " Verbose !!!" else ""
    logInfo $ "We're inside the application!" <> suffix
