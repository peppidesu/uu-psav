{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import CLI (getPlain, getInputs, usage, Options, CLIGeneric(..), PlainData)

main = do
     inputs <- getPlain
     if inputs.showHelp
     then putStrLn usage
     else print inputs

