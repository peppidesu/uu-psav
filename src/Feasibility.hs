module Feasibility where
import Types
import GCLParser.GCLDatatype (Expr(..), opAnd)
import Data.Tree (Tree(..))
import WLP(wlp')
import Debug.Trace
import Optimize (optimize)
import SMT (VarTypes, z3Check)
import GHC.IO (unsafePerformIO)
import Data.List.Extra (firstJust)
import Data.Maybe (isJust)

-- | Takes the PathTree to calculate the wlp for, and a list of statements that is used in the recursion that should start empty.
-- Check the feasibility for every assume statement it encounters, and stops wlp calculation for anything infeasible.
wlpAlwaysFeasibilityCheck ::  VarTypes -> [Stmt] -> PathTree -> [Expr]
wlpAlwaysFeasibilityCheck types prefix (Node (Assume e) [])
    | check types e prefix = [wlp' (Assume e) (LitB True)  ]
    | otherwise            = []
wlpAlwaysFeasibilityCheck _     _      (Node x [])
    = [wlp' x (LitB True)] -- prefix does not need to be changed since it is not passed anywhere
wlpAlwaysFeasibilityCheck types prefix (Node (Assume e) ts)
    | check types e prefix = map (wlp' (Assume e)) (concat $ map (wlpAlwaysFeasibilityCheck types ((Assume e) : prefix)) ts)
    | otherwise            = []

-- All asserts should be ignored for feasibility checking.
wlpAlwaysFeasibilityCheck types prefix (Node (Assert e) ts)
    = map (wlp' (Assert e)) (concat $ map (wlpAlwaysFeasibilityCheck types prefix      ) ts)
wlpAlwaysFeasibilityCheck types prefix (Node x ts)
    = map (wlp' x)          (concat $ map (wlpAlwaysFeasibilityCheck types (x : prefix)) ts)

-- | Checks if the program with the list of statements (which should be given in reverse), and the given post condition (which should not be
--  included in the statement list) is feasible.
check :: VarTypes -> Expr -> [Stmt] -> Bool
check types e ts = simpleCheck . optimize $ wlpListRevCon ts e
    where
    simpleCheck (LitB False) = False
    -- TODO: remove unsafePerformIO
    simpleCheck expr = isJust $ unsafePerformIO $ z3Check types expr
-- | Calculates the wlp for the program consisting of the list of statements, which is listed in reverse order. The given expression is the required post condition.
-- The wlp is calculated conjunctively with regards to assume.
wlpListRevCon :: [Stmt] -> Expr -> Expr
wlpListRevCon []     e = e
wlpListRevCon (t:ts) e = wlpListRevCon ts (wlpCon t e)



-- | Calculates the wlp, which is conjunctive with regards to assume.
wlpCon :: Stmt -> Expr -> Expr
wlpCon (Assume e) q = opAnd e q
wlpCon e          q = wlp'  e q
