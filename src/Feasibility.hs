module Feasibility where
import Types
import GCLParser.GCLDatatype (Expr(..), opAnd)
import Data.Tree (Tree(..))
import WLP(wlp')
import Optimize (optimize)
import SMT (VarTypes, z3Check, Assignments, z3CheckBool)
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
check :: Int -> VarTypes -> Expr -> [Stmt] -> AppM Bool
check size types e ts = do
    verbose <- asksConfig cfgVerbose
    k <- asksConfig cfgK
    q <- asksConfig cfgQ
    rand <- getRandom::AppM Double
    predicate <- optimize $ wlpConj ts e
    result <- if rand > checkProb q k ((length ts))
        then fromMaybe True
            <$> simpleCheck predicate
        else fromMaybe
            <$> (liftIO $ z3CheckBool types predicate)
            <*> (simpleCheck predicate)
    addMetrics mempty {
        mtrFeasibilityChecks = 1,
        mtrFeasibilityPruned = fromBool (not result),
        mtrFeasibilityPrunedAmount = if not result then size else 0
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
                mtrFeasibilityPrunedTrivial = fromBool (not a),
                mtrFeasibilityPrunedAmountTrivial = if not a then size else 0
            }
            pure $ Just a
        simpleCheck _ = pure Nothing

findCounterExample :: VarTypes -> PathTree -> AppM (Maybe Assignments)
findCounterExample types = go [] id
    where
    -- k maps a leaf's base wlp to the full path wlp, accumulated from ancestors
    go :: [Stmt] -> (Expr -> Expr) -> PathTree -> AppM (Maybe Assignments)
    go prefix k node@(Node (Assume e) []) = do
        ok <- check (pathCount node) types e prefix
        if ok
            then verify types (k (wlp' (Assume e) (LitB True)))
            else pure Nothing

    go _ k (Node x []) = verify types (k (wlp' x (LitB True)))

    go prefix k node@(Node (Assume e) ts) = do
        ok <- check (pathCount node) types e prefix
        if ok
            then firstJustM (go (Assume e : prefix) (k . wlp' (Assume e))) ts
            else pure Nothing

    go prefix k (Node x ts) = firstJustM (go (x : prefix) (k . wlp' x)) ts

    verify t e = do
        opt <- optimize e
        case opt of
            LitB _ -> pure (Just $ [("x", "any")])
            expr -> liftIO (z3Check t (OpNeg expr))
