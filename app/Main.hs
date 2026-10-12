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
    let paths = unrollUntil 50 $ G.stmt prog
    return $ concatMap (map optimize . flip wlpAlwaysFeasibilityCheck []) paths

main :: IO ()
main = let
 in do
    exprs <- test
    let optimized = map optimize exprs
    _ <- mapM (putStrLn . (++ "\n") . show . G.PP.ppExpr) optimized
    -- _ <- mapM (optimizeTrace) (zip exprs [0..])
    return ()
