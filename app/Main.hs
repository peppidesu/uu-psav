module Main (main) where

import GCLParser.Parser

import GCLParser.GCLDatatype (Expr)

import qualified GCLParser.GCLDatatype as G
import qualified GCLParser.PrettyPrint as G.PP

import Control.Monad
import WLP
import Unroll (unrollUntil)
import Feasibility (wlpAlwaysFeasibilityCheck)
import Optimize
import PrettyPrint (ppPathTree2String)

test :: IO [Expr]
test = do
    Right(prog) <- parseGCLfile "./examples/benchmark/bsort.gcl"
    let paths = unrollUntil 30 $ G.stmt prog
    return $ concatMap (flip wlpAlwaysFeasibilityCheck []) paths

main :: IO ()
main = let
 in do
    exprs <- test
    let optimized = map optimizeUntilIdempotence exprs
    _ <- mapM (putStrLn . show . G.PP.ppExpr) optimized
    -- _ <- mapM (optimizeTrace) (zip exprs [0..])
    return ()
