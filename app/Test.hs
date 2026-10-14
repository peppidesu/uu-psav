{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import CLI (getPlain, getInputs, usage, Options, CLIGeneric(..), PlainData)
import GCLParser.Parser
import Unroll (unrollUntil)
import qualified GCLParser.GCLDatatype as G
import Optimize
import Feasibility
import qualified GCLParser.PrettyPrint as G.PP
import CLI
import SMT
import Data.List.Extra (firstJust)






main = do
     inputs@(CLI {plength = k, conf = conf, gcl = gclPrograms, showHelp = showHelp'}) <- getPlain
     if inputs.showHelp
     then putStrLn usage
     else run (inputs)

run :: PlainData -> IO ()
run (CLI {
                   plength,
                   conf,
                   gcl,
                   showHelp}   ) = do
    let Right(prog) = head gcl
    let paths = unrollUntil (fromIntegral plength) $ G.stmt prog
    let varTypes = (getTypes prog)
    putStrLn $ "varTypes = " ++ show varTypes
    let exprs = concatMap (wlpAlwaysFeasibilityCheck varTypes []) paths
    let optimized = map optimize exprs
    maybeCounterExample <- firstJust id <$> mapM (z3Check varTypes) optimized
    case maybeCounterExample of
      Just counterexample -> putStrLn $ "Found a counterexample: " ++ show counterexample
      Nothing             -> putStrLn $ ("Up to length K = " ++ show plength ++ ", all paths are valid")


