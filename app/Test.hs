{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import CLI (getPlain, getInputs, usage, Options, CLIGeneric(..), PlainData)

main = do
     inputs <- getPlain
     if inputs.showHelp
     then print usage
     else print inputs
