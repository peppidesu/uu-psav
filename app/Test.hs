{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import CLI (getPlain, getInputs, usage, Options, CLIGeneric(..), PlainData)


data Expr a where
 VarInt :: String -> Expr Int
 VarBool :: String -> Expr Bool
 VarAInt ;: String -> Expr (Array Int)
 VarABool :: String -> Expr (Array Bool)
 
 ArrayElem :: (Expr (Array a)) -> Expr Int -> Expr a





main = do
     inputs@(CLI {plength = k, conf = conf, gcl = gclPrograms, showHelp = showHelp'}) <- getPlain
     if inputs.showHelp
     then putStrLn usage
     else print inputs

