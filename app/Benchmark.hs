{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

import Criterion.Main
import qualified ShadowElimination as S
import SMT (getTypes)
import Unroll (unrollUntil)
import Feasibility (wlpAlwaysFeasibilityCheck)
import Optimize (optimize)
import SMT (z3Check)
import Control.Monad.Extra (firstJustM)
import GCLParser.GCLDatatype as G
import GCLParser.Parser (parseGCLfile)
import Control.DeepSeq
import GHC.Generics (Generic)
import Control.Exception (evaluate)

-- The env function of criterion requires program to be NFData. We do not have an easy way to change the 
-- file program is defined in, so there is an instance declaration here.
instance Generic Program
instance NFData Program where rnf x = seq x ()


gclfileToProgram path = do
  (Right maybeProgram) <- parseGCLfile path
  return maybeProgram

-- Our benchmark harness.
main = do 
  prog <-  (gclfileToProgram "./examples/E.gcl")
  prog' <- evaluate prog
  defaultMain [
    env (gclfileToProgram "./examples/benchmark/bsort.gcl") $ \program -> 
      bgroup "bsort" [
        bench "10" $ nfIO $ run program 10
        ]
    ]

run :: G.Program -> Int -> IO ()
run prog' k = do
    let (prog,(_, _, renamingRecovery)) = S.shadowElim prog'
    let paths = unrollUntil (fromIntegral k) $ G.stmt prog
    let varTypes = (getTypes prog)
    let exprs = concatMap (wlpAlwaysFeasibilityCheck varTypes []) paths
    let optimized = map optimize exprs
    maybeCounterExample <- firstJustM (z3Check varTypes . G.OpNeg) optimized
    return ()