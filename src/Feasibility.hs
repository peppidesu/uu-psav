module Feasibility where
import Types
import GCLParser.GCLDatatype (Expr(..), opAnd)
import Data.Tree (Tree(..))
import WLP(wlp')
import Optimize (optimize)
import SMT (VarTypes, z3Check)
import Data.Maybe (isJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Extra (concatMapM, firstJustM)
import Debug.Trace (trace)

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
check :: VarTypes -> Expr -> [Stmt] -> AppM Bool
check types e ts = do
    verbose <- asksConfig cfgVerbose

    predicate <- optimize $ wlpConj ts e
    result <- simpleCheck predicate

    pure $ if verbose then trace (unlines [
        "Feasibility: " ++ show predicate,
        "  = " ++ show result
     ]) result else result

    where
        simpleCheck (LitB a) = pure a
        simpleCheck expr = liftIO (isJust <$> z3Check types expr)

findCounterExample :: VarTypes -> PathTree -> AppM (Maybe Expr)
findCounterExample types = go [] id
    where
    -- k maps a leaf's base wlp to the full path wlp, accumulated from ancestors
    go :: [Stmt] -> (Expr -> Expr) -> PathTree -> AppM (Maybe Expr)
    go prefix k (Node (Assume e) []) = do
        ok <- check types e prefix
        if ok
            then verify types (k (wlp' (Assume e) (LitB True)))
            else pure Nothing

    go _ k (Node x []) = verify types (k (wlp' x (LitB True)))

    go prefix k (Node (Assume e) ts) = do
        ok <- check types e prefix
        if ok
            then firstJustM (go (Assume e : prefix) (k . wlp' (Assume e))) ts
            else pure Nothing

    go prefix k (Node x ts) = firstJustM (go (x : prefix) (k . wlp' x)) ts

    verify t e = do
        opt <- optimize e                                    -- AppM Expr, as now
        res <- liftIO (z3Check t (OpNeg opt))
        pure (if isJust res then Just opt else Nothing)
