module Feasibility where
import Types
import GCLParser.GCLDatatype (Expr(..), opAnd, BinOp (LessThan))
import Data.Tree (Tree(..))
import WLP(wlp')
import Debug.Trace
import Optimize (optimize)
import SMT (VarTypes, z3Check)
import GHC.IO (unsafePerformIO)
import Data.Maybe (isJust)

-- | Calculates the wlp for the program consisting of the list of statements, which is listed in reverse order. The given expression is the required post condition.
-- The wlp is calculated conjunctively with regards to assume.
wlpConj :: [Stmt] -> Expr -> Expr
wlpConj []     e = e
wlpConj (t:ts) e = wlpConj ts (wlpConj' t e)

wlpConj' :: Stmt -> Expr -> Expr
wlpConj' (Assume e) q = opAnd e q   -- treat assume as assert (conjunction)
wlpConj' (Assert _) q = q           -- ignore asserts
wlpConj' e          q = wlp'  e q

-- | Checks if the program with the list of statements (which should be given in reverse), and the given post condition (which should not be
--  included in the statement list) is feasible.
check :: VarTypes -> Expr -> [Stmt] -> Bool
check types e ts = trace (unlines [
        "Feasibility: " ++ show predicate,
        -- "Feasibility (unsimplified): " ++ show (wlpConj ts e),
        "   = " ++ show result
    ]) result
    where
    predicate = optimize $ wlpConj ts e
    result = unsafePerformIO $ simpleCheck predicate

    -- soundness | result /= (unsafePerformIO $ simpleCheck $ wlpConj ts e) = error ""
    --           | otherwise = ()

    simpleCheck (LitB a) = pure a
    simpleCheck expr = isJust <$> z3Check types expr

-- | Takes the PathTree to calculate the wlp for, and a list of statements that is used in the recursion that should start empty.
-- Check the feasibility for every assume statement it encounters, and stops wlp calculation for anything infeasible.
wlpAlwaysFeasibilityCheck ::  VarTypes -> [Stmt] -> PathTree -> [Expr]
wlpAlwaysFeasibilityCheck types prefix (Node (Assume e) [])
    | check types e prefix = [wlp' (Assume e) (LitB True)]
    | otherwise                              = []
wlpAlwaysFeasibilityCheck _     _      (Node x [])
    = [wlp' x (LitB True)]

wlpAlwaysFeasibilityCheck types prefix (Node (Assume e) ts)
    | check types e prefix = map (wlp' (Assume e)) $ concatMap (wlpAlwaysFeasibilityCheck types ((Assume e) : prefix)) ts
    | otherwise                              = []

wlpAlwaysFeasibilityCheck types prefix (Node x ts)
    = map (wlp' x) (concatMap (wlpAlwaysFeasibilityCheck types (x : prefix)) ts)
