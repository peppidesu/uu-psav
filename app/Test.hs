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
    let exprs = concatMap (map optimize . flip wlpAlwaysFeasibilityCheck []) paths
    let optimized = map optimize exprs
    _ <- mapM (putStrLn . (++ "\n") . show . G.PP.ppExpr) optimized
    -- _ <- mapM (optimizeTrace) (zip exprs [0..])
    return ()


