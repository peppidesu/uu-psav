module Feasibility where
import Types
import GCLParser.GCLDatatype (Expr(..), opAnd)
import Data.Tree (Tree(..))
import WLP(wlp')

-- | Takes the PathTree to calculate the wlp for, and a list of statements that is used in the recursion that should start empty.
-- Check the feasibility for every assume statement it encounters, and stops wlp calculation for anything infeasible.
wlpAlwaysFeasibilityCheck :: PathTree -> [Stmt] -> [Expr]
wlpAlwaysFeasibilityCheck (Node (Assume e) []) prefix | check e prefix = [wlp' (Assume e) (LitB True)  ]                   
                                                      | otherwise      = []
wlpAlwaysFeasibilityCheck (Node x []) _                                = [wlp' x (LitB True)] -- prefix does not need to be changed since it is not passed anywhere
wlpAlwaysFeasibilityCheck (Node (Assume e) ts) prefix 
                                   | check e prefix    = map (wlp' (Assume e)) (concat $ map (flip wlpAlwaysFeasibilityCheck ((Assume e) : prefix)) ts)                            
                                   | otherwise         = []

-- All asserts should be ignored for feasibility checking.
wlpAlwaysFeasibilityCheck (Node (Assert e) ts) prefix  = map (wlp' (Assert e)) (concat $ map (flip wlpAlwaysFeasibilityCheck prefix)                ts)
wlpAlwaysFeasibilityCheck (Node x ts) prefix           = map (wlp' x)          (concat $ map (flip wlpAlwaysFeasibilityCheck (x : prefix))          ts)

-- | Checks if the program with the list of statements (which should be given in reverse), and the given post condition (which should not be 
--  included in the statement list) is feasible.
check :: Expr -> [Stmt] -> Bool
check e [] = z3Check e
check e ts = z3Check $ wlpListRevCon ts e

-- | Calculates the wlp for the program consisting of the list of statements, which is listed in reverse order. The given epxression is the required post condition.
-- The wlp is calculated conjunctively with regards to assume.
wlpListRevCon :: [Stmt] -> Expr -> Expr
wlpListRevCon []     e = e
wlpListRevCon (t:ts) e = wlpListRevCon ts (wlpCon t e)

z3Check :: Expr -> Bool
z3Check e = seq e True

-- | Calculates the wlp, which is conjunctive with regards to assume.
wlpCon :: Stmt -> Expr -> Expr
wlpCon (Assume e) q = opAnd e q
wlpCon e          q = wlp'  e q