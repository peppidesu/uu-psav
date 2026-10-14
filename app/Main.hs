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
import SMT
import PrettyPrint (ppPathTree2String)
import qualified Data.Map.Strict as Map
import Data.List.Extra (firstJust)

test :: IO (VarTypes, [Expr])
test = do
    Right(prog) <- parseGCLfile "./examples/benchmark/bsort.gcl"
    let paths = unrollUntil 50 $ G.stmt prog
    let varTypes = (getTypes prog)
    putStrLn $ "varTypes = " ++ show varTypes
    return $ (,) varTypes $ concatMap (wlpAlwaysFeasibilityCheck varTypes []) paths

main :: IO ()
main =
  do (varTypes, exprs) <- test
     let optimized = map optimize exprs
     maybeCounterExample <- firstJust id <$> mapM (z3Check varTypes) (map G.OpNeg optimized)
     case maybeCounterExample of
       Just counterexample -> putStrLn $ "Found a counterexample: " ++ show counterexample
       Nothing             -> putStrLn $ "Up to length K = 30, all paths are valid"
     -- _ <- mapM (putStrLn . (++ "\n") . show . G.PP.ppExpr) optimized
     -- _ <- mapM (optimizeTrace) (zip exprs [0..])
