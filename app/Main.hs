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
import qualified ShadowElimination as S
import Control.Monad.Extra (firstJustM, concatMapM)
import Types (AppM)
import Control.Monad.IO.Class (MonadIO(liftIO))

main :: AppM ()
main = do
     inputs@(CLI {plength = k, conf = conf, gcl = gclPrograms, showHelp = showHelp'}) <- getPlain
     if inputs.showHelp
     then liftIO $ putStrLn usage
     else run (inputs)

run :: PlainData -> AppM ()
run (CLI {
                   plength,
                   conf,
                   gcl,
                   showHelp}   ) = do
    let Right(prog') = head gcl
    let (prog,(_, _, renamingRecovery)) = S.shadowElim prog'
    let paths = unrollUntil (fromIntegral plength) $ G.stmt prog
    let varTypes = (getTypes prog)
    exprs <- concatMapM (wlpAlwaysFeasibilityCheck varTypes []) paths
    optimized <- mapM optimize exprs
    maybeCounterExample <- liftIO $ firstJustM (z3Check varTypes . G.OpNeg) optimized
    case maybeCounterExample of
      Just counterexample -> liftIO $ putStrLn $ "Found a counterexample: " ++ show counterexample
      Nothing             -> liftIO $ putStrLn $ ("Up to length K = " ++ show plength ++ ", all paths are valid")
