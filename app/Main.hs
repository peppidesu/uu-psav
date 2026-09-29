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

test :: IO [Expr]
test = do
    Right(prog) <- parseGCLfile "./examples/reverse.gcl"
    let paths = unrollUntil 20 $ G.stmt prog
    return $ concatMap (flip wlpAlwaysFeasibilityCheck []) paths

optimizeUntilIdempotence :: Expr -> Expr
optimizeUntilIdempotence exprs = let
    optimized = foldr1 (.) (map foldExpr [
        exprVerbEqAlgebra,
        exprTautologyAlgebra,
        exprIntCalcAlgebra,
        exprRewriteAlgebra
     ]) exprs;
 in if optimized == exprs then exprs else optimizeUntilIdempotence optimized

optimizeTrace :: (Expr, Int) -> IO ()
optimizeTrace (exprs, index) = let
    optimized = foldr1 (.) (map foldExpr [
        exprVerbEqAlgebra,
        exprTautologyAlgebra,
        exprIntCalcAlgebra,
        exprRewriteAlgebra
     ]) exprs;
 in do
  _ <- putStrLn $ show $ G.PP.ppExpr exprs
  _ <- putStrLn (show index ++"-----")
  when (optimized /= exprs) $ optimizeTrace (optimized, index)

main :: IO ()
main = let
 in do
    exprs <- test
    -- let optimized = map optimizeUntilIdempotence exprs
    -- _ <- mapM (putStrLn . show . G.PP.ppExpr) optimized
    _ <- mapM (optimizeTrace) (zip exprs [0..])
    return ()
