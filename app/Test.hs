{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import CLI (getPlain, getInputs, usage, Options, CLIGeneric(..), PlainData)






main = do
     inputs@(CLI {plength = k, conf = conf, gcl = gclPrograms, showHelp = showHelp'}) <- getPlain
     if inputs.showHelp
     then putStrLn usage
     else print inputs

