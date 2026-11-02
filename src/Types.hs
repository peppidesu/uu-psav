{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
module Types (
    Stmt(..),
    PathTree,
    ExprF(..),
    embed,
    ExprAlgebra,
    foldExpr,
    AppM(..),
    Config(..),
    Metrics(..),
    addMetrics,
    asksConfig,
    runApp,
    pathCount,
    exprSize,
    MetricsAvStd,
    toMetricsAvStd
) where

import Data.Tree (Tree (..))
import qualified GCLParser.GCLDatatype as G
import qualified Data.Functor.Foldable
import Data.Functor.Foldable (cata)
import Data.Map (Map)
import GCLParser.GCLDatatype (Expr)
import Control.Lens.Plated (Plated (plate))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State.Lazy (MonadState (..), StateT, runStateT, modify)
import Data.Time.Clock (NominalDiffTime)
import Data.Time (getCurrentTime, diffUTCTime)
import Control.Monad.Random (StdGen, RandT, MonadRandom, mkStdGen, evalRandT, initStdGen, evalRand, random)
import GHC.Generics (Generic)
import GHC.Clock (getMonotonicTimeNSec)
import Data.Word (Word64)
import Data.Ratio ((%))
import Extra (intToDouble)

data Stmt
    = Skip
    | Assert      G.Expr
    | Assume      G.Expr
    | Assign      String           G.Expr
    | AAssign     String           G.Expr   G.Expr

instance Show Stmt where
    show Skip                   = "skip"
    show (Assert cond)          = "assert " ++ show cond
    show (Assume cond)          = "assume " ++ show cond
    show (Assign ident e)       = ident ++ " := " ++ show e
    show (AAssign ident i e)    = ident ++ "[" ++ show i ++ "]" ++ " := " ++ show e

exprSize :: Expr -> Int
exprSize = cata $ \case
    VarF _ -> 1
    LitBF _ -> 1
    LitIF _ -> 1
    LitNullF -> 1
    DereferenceF _ -> 1
    e -> foldr (+) 0 e

-- |Tree of program paths. Each node is a statement, and its children
-- are next statements that can be executed. Exploring to a leaf node produces
-- a program path.
type PathTree = Tree Stmt

pathCount :: PathTree -> Int
pathCount (Node _ []) = 1
pathCount (Node _ xs) = sum (map pathCount xs)

-- https://wiki.haskell.org/Catamorphisms
data ExprF a
    = VarF         String
    | LitIF        Int
    | LitBF        Bool
    | LitNullF
    | ParensF      a
    | ArrayElemF   a a
    | OpNegF       a
    | BinopExprF   G.BinOp a a
    | ForallF      String a
    | ExistsF      String a
    | SizeOfF      a
    | RepByF       a a a
    | CondF        a a a
    | NewStoreF    a
    | DereferenceF String
  deriving (Eq, Show, Functor, Foldable)

type instance Data.Functor.Foldable.Base G.Expr = ExprF

instance Data.Functor.Foldable.Recursive G.Expr where
    project = \case
        G.Var s             -> VarF s
        G.LitI n            -> LitIF n
        G.LitB b            -> LitBF b
        G.LitNull           -> LitNullF
        G.Parens e          -> ParensF e
        G.ArrayElem e1 e2   -> ArrayElemF e1 e2
        G.OpNeg e           -> OpNegF e
        G.BinopExpr b e1 e2 -> BinopExprF b e1 e2
        G.Forall s e        -> ForallF s e
        G.Exists s e        -> ExistsF s e
        G.SizeOf e          -> SizeOfF e
        G.RepBy e1 e2 e3    -> RepByF e1 e2 e3
        G.Cond e1 e2 e3     -> CondF e1 e2 e3
        G.NewStore e        -> NewStoreF e
        G.Dereference s     -> DereferenceF s

embed :: ExprF G.Expr -> G.Expr
embed = \case
    VarF s             -> G.Var s
    LitIF n            -> G.LitI n
    LitBF b            -> G.LitB b
    LitNullF           -> G.LitNull
    ParensF e          -> G.Parens e
    ArrayElemF e1 e2   -> G.ArrayElem e1 e2
    OpNegF e           -> G.OpNeg e
    BinopExprF b e1 e2 -> G.BinopExpr b e1 e2
    ForallF s e        -> G.Forall s e
    ExistsF s e        -> G.Exists s e
    SizeOfF e          -> G.SizeOf e
    RepByF e1 e2 e3    -> G.RepBy e1 e2 e3
    CondF e1 e2 e3     -> G.Cond e1 e2 e3
    NewStoreF e        -> G.NewStore e
    DereferenceF s     -> G.Dereference s

instance Plated G.Expr where
    plate f = \case
        G.Var s             -> pure (G.Var s)
        G.LitI n            -> pure (G.LitI n)
        G.LitB b            -> pure (G.LitB b)
        G.LitNull           -> pure G.LitNull
        G.Dereference s     -> pure (G.Dereference s)
        G.Parens e          -> G.Parens <$> f e
        G.ArrayElem e1 e2   -> G.ArrayElem <$> f e1 <*> f e2
        G.OpNeg e           -> G.OpNeg <$> f e
        G.BinopExpr b e1 e2 -> G.BinopExpr b <$> f e1 <*> f e2
        G.Forall s e        -> G.Forall s <$> f e
        G.Exists s e        -> G.Exists s <$> f e
        G.SizeOf e          -> G.SizeOf <$> f e
        G.RepBy e1 e2 e3    -> G.RepBy <$> f e1 <*> f e2 <*> f e3
        G.Cond e1 e2 e3     -> G.Cond <$> f e1 <*> f e2 <*> f e3
        G.NewStore e        -> G.NewStore <$> f e

type ExprAlgebra a = ExprF a -> a

foldExpr :: ExprAlgebra a -> G.Expr -> a
foldExpr = cata

data Config = Config
    { cfgVerbose     :: !Bool
    , cfgSimplifierEnabled :: !Bool
    , cfgSimplifierBoolConstFolding :: !Bool
    , cfgSimplifierChainAbsorb :: !Bool
    , cfgSimplifierChainDedup :: !Bool
    , cfgSimplifierChainTopBottom :: !Bool
    , cfgSimplifierChainComparison :: !Bool
    , cfgAlwaysSimpleCheck :: !Bool
    , cfgHeuristic :: !Bool
    , cfgK :: !Int
    , cfgQ :: !Double
    }
    deriving stock (Show, Eq, Generic)

asksConfig :: (Config -> a) -> AppM a
asksConfig = asks

newtype AppM a = AppM {unAppM ::
    ReaderT Config  (
    StateT  Metrics (
    RandT   StdGen
    IO)) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Config
    , MonadState  Metrics
    , MonadRandom
    )

-- | Run the app, returning both the result and the collected metrics.
runApp :: Config -> AppM a -> IO (a, Metrics)
runApp cfg (AppM m) = do
    rng <- initStdGen
    evalRandT (runStateT (runReaderT m cfg) emptyMetrics) rng

-- | Run the app and discard the metrics.
runApp_ :: Config -> AppM a -> IO a
runApp_ cfg app = fst <$> runApp cfg app


data Metrics = Metrics
    {
      mtrFeasibilityChecks :: !Int
    , mtrFeasibilityZ3 :: !Int
    , mtrFeasibilityTrivial :: !Int
    , mtrFeasibilityPrunedZ3 :: !Int
    , mtrFeasibilityPrunedTrivial :: !Int
    , mtrFormulaLength :: !Int
    , mtrFeasibilityPrunedAmountZ3 :: !Int
    , mtrFeasibilityPrunedAmountTrivial :: !Int
    , mtrSimplifierElapsed :: !NominalDiffTime
    , mtrSimplifierCalls :: !Int
    }
    deriving stock (Show, Eq)

nsToDiffTime :: Word64 -> NominalDiffTime
nsToDiffTime ns = fromRational (toInteger ns % 1000000000)

timeSimplifier :: AppM a -> AppM a
timeSimplifier act = do
    t0 <- liftIO getMonotonicTimeNSec
    !a <- act
    t1 <- liftIO getMonotonicTimeNSec
    addMetrics mempty {
        mtrSimplifierCalls = 1,
        mtrSimplifierElapsed = nsToDiffTime (t1 - t0)
    }
    pure a

emptyMetrics :: Metrics
emptyMetrics = Metrics 0 0 0 0 0 0 0 0 0 0

addMetrics :: Metrics -> AppM ()
addMetrics m = modify (<> m)

-- Metrics form a monoid, so sub-computations can be measured and merged.
instance Semigroup Metrics where
    a <> b = Metrics
        { mtrFeasibilityChecks = mtrFeasibilityChecks a + mtrFeasibilityChecks b
        , mtrFeasibilityZ3 = mtrFeasibilityZ3 a + mtrFeasibilityZ3 b
        , mtrFeasibilityTrivial = mtrFeasibilityTrivial a + mtrFeasibilityTrivial b
        , mtrFeasibilityPrunedZ3 = mtrFeasibilityPrunedZ3 a + mtrFeasibilityPrunedZ3 b
        , mtrFeasibilityPrunedTrivial = mtrFeasibilityPrunedTrivial a  + mtrFeasibilityPrunedTrivial b
        , mtrFeasibilityPrunedAmountZ3 = mtrFeasibilityPrunedAmountZ3 a + mtrFeasibilityPrunedAmountZ3 b
        , mtrFormulaLength = mtrFormulaLength a + mtrFormulaLength b
        , mtrFeasibilityPrunedAmountTrivial = mtrFeasibilityPrunedAmountTrivial a + mtrFeasibilityPrunedAmountTrivial b
        , mtrSimplifierElapsed = mtrSimplifierElapsed a + mtrSimplifierElapsed b
        , mtrSimplifierCalls = mtrSimplifierCalls a + mtrSimplifierCalls b
        }

instance Monoid Metrics where
    mempty = emptyMetrics

stdev :: Floating a => [a] -> a
stdev xs = sqrt . average . map ((^2) . (-) axs) $ xs
    where average = (/) <$> sum <*> realToFrac . length
          axs     = average xs

average :: Floating a => [a] -> a
average xs = sum xs / (realToFrac $ length xs)

data MetricsAvStd = MetricsAvStd
    {
      mtrAvStdFeasibilityChecks :: !Double
    , mtrAvStdFeasibilityZ3 :: !Double
    , mtrAvStdFeasibilityTrivial :: !Double
    , mtrAvStdFeasibilityPrunedZ3 :: !Double
    , mtrAvStdFeasibilityPrunedTrivial :: !Double
    , mtrAvStdFormulaLength :: !Double
    , mtrAvStdFeasibilityPrunedAmountZ3 :: !Double
    , mtrAvStdFeasibilityPrunedAmountTrivial :: !Double
    , mtrAvStdSimplifierTime :: !Double
    }
    deriving stock (Show, Eq)

toMetricsAvStd :: [Metrics] -> (MetricsAvStd, MetricsAvStd)
toMetricsAvStd ms = (
    MetricsAvStd {
        mtrAvStdFeasibilityChecks               = stdev $ map (intToDouble . mtrFeasibilityChecks) ms,
        mtrAvStdFeasibilityZ3                   = stdev $ map (intToDouble . mtrFeasibilityZ3) ms,
        mtrAvStdFeasibilityTrivial              = stdev $ map (intToDouble . mtrFeasibilityTrivial) ms,
        mtrAvStdFeasibilityPrunedZ3             = stdev $ map (intToDouble . mtrFeasibilityPrunedZ3) ms,
        mtrAvStdFeasibilityPrunedTrivial        = stdev $ map (intToDouble . mtrFeasibilityPrunedTrivial) ms,
        mtrAvStdFormulaLength                   = stdev $ map (intToDouble . mtrFormulaLength) ms,
        mtrAvStdFeasibilityPrunedAmountZ3       = stdev $ map (intToDouble . mtrFeasibilityPrunedAmountZ3) ms,
        mtrAvStdFeasibilityPrunedAmountTrivial  = stdev $ map (intToDouble . mtrFeasibilityPrunedAmountTrivial) ms,
        mtrAvStdSimplifierTime                  = stdev $ map (\m ->
            (realToFrac $ mtrSimplifierElapsed m) / (realToFrac $ mtrSimplifierCalls m)
        ) ms
     },
    MetricsAvStd {
        mtrAvStdFeasibilityChecks               = average $ map (intToDouble . mtrFeasibilityChecks) ms,
        mtrAvStdFeasibilityZ3                   = average $ map (intToDouble . mtrFeasibilityZ3) ms,
        mtrAvStdFeasibilityTrivial              = average $ map (intToDouble . mtrFeasibilityTrivial) ms,
        mtrAvStdFeasibilityPrunedZ3             = average $ map (intToDouble . mtrFeasibilityPrunedZ3) ms,
        mtrAvStdFeasibilityPrunedTrivial        = average $ map (intToDouble . mtrFeasibilityPrunedTrivial) ms,
        mtrAvStdFormulaLength                   = average $ map (intToDouble . mtrFormulaLength) ms,
        mtrAvStdFeasibilityPrunedAmountZ3       = average $ map (intToDouble . mtrFeasibilityPrunedAmountZ3) ms,
        mtrAvStdFeasibilityPrunedAmountTrivial  = average $ map (intToDouble . mtrFeasibilityPrunedAmountTrivial) ms,
        mtrAvStdSimplifierTime                  = average $ map (\m ->
            (realToFrac $ mtrSimplifierElapsed m) / (realToFrac $ mtrSimplifierCalls m)
        ) ms
     }
 )
