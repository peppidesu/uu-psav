module Feasibility where
import Types
import GCLParser.GCLDatatype (Expr(..), opAnd)
import Data.Tree (Tree(..))
import WLP(wlp')
import Optimize (optimize)
import SMT (VarTypes, Assignments, Z3Env, newZ3Env, z3CheckIn, z3CheckBoolIn)
import Data.Maybe (isJust, fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Extra (concatMapM, firstJustM)
import Debug.Trace (trace)
import Control.Monad.Random (MonadRandom(getRandom))
import GHC.Float (int2Double)
import Control.Monad.State.Lazy (modify)
import Foreign (fromBool)

-- | Calculates the wlp for the program consisting of the list of statements, which is listed in reverse order. The given expression is the required post condition.
-- The wlp is calculated conjunctively with regards to assume.
wlpConj :: [Stmt] -> Expr -> Expr
wlpConj []     e = e
wlpConj (t:ts) e = wlpConj ts (wlpConj' t e)

wlpConj' :: Stmt -> Expr -> Expr
wlpConj' (Assume e) q = opAnd e q   -- treat assume as assert (conjunction)
wlpConj' (Assert _) q = q           -- ignore asserts
wlpConj' e          q = wlp'  e q

checkProb :: Double -> Int -> Int -> Double
checkProb q k x | q == 0 = 1 - b
                | otherwise = (a ** b - a) / (1 - a)
    where
        a = 2 ** (-q)
        b = int2Double x / int2Double k
-- | Checks if the program with the list of statements (which should be given in reverse), and the given post condition (which should not be
--  included in the statement list) is feasible.
check :: Z3Env -> Int -> VarTypes -> Expr -> [Stmt] -> AppM Bool
check env size types e ts = do
    verbose <- asksConfig cfgVerbose
    k <- asksConfig cfgK
    q <- asksConfig cfgQ
    alwaysSimpleCheck <- asksConfig cfgAlwaysSimpleCheck
    heuristic <- asksConfig cfgHeuristic

    predicate <- optimize $ wlpConj ts e

    let prob = checkProb q k (length ts)
    rand <- getRandom::AppM Double

    result <- if heuristic && rand > prob
        then if alwaysSimpleCheck
            then fromMaybe True <$> simpleCheck predicate
            else pure True
        else fromMaybe
            <$> (do
                a <- liftIO $ z3CheckBoolIn env types predicate
                addMetrics mempty {
                    mtrFeasibilityZ3 = 1,
                    mtrFeasibilityPrunedZ3 = fromBool (not a),
                    mtrFormulaLength = exprSize predicate
                }
                return a
            )
            <*> (simpleCheck predicate)
    addMetrics mempty {
        mtrFeasibilityChecks = 1
    }
    pure $ if verbose then trace (unlines [
        "Feasibility: " ++ show predicate,
        "  = " ++ show result
        ]) result else result

    where
        simpleCheck :: Expr -> AppM (Maybe Bool)
        simpleCheck (LitB a) = do
            addMetrics mempty {
                mtrFeasibilityTrivial = 1,
                mtrFeasibilityPrunedTrivial = fromBool (not a)
            }
            pure $ Just a
        simpleCheck _ = pure Nothing

findCounterExample :: VarTypes -> PathTree -> AppM (Maybe Assignments)
findCounterExample types tree = do
    env <- liftIO newZ3Env
    go env [] id tree
    where
    -- k maps a leaf's base wlp to the full path wlp, accumulated from ancestors
    go :: Z3Env -> [Stmt] -> (Expr -> Expr) -> PathTree -> AppM (Maybe Assignments)
    go env prefix k node@(Node (Assume e) []) = do
        ok <- check env (pathCount node) types e prefix
        if ok
            then verify env types (k (wlp' (Assume e) (LitB True)))
            else pure Nothing

    go env _ k (Node x []) = verify env types (k (wlp' x (LitB True)))

    go env prefix k node@(Node (Assume e) ts) = do
        ok <- check env (pathCount node) types e prefix
        if ok
            then firstJustM (go env (Assume e : prefix) (k . wlp' (Assume e))) ts
            else pure Nothing

    go env prefix k (Node x ts) = firstJustM (go env (x : prefix) (k . wlp' x)) ts

    verify env t e = do
        opt <- optimize e
        addMetrics mempty {
            mtrPathsInspected = 1
        }
        case opt of
            LitB False -> pure (Just $ [("x", "any")])
            expr -> liftIO (z3CheckIn env t (OpNeg expr))
