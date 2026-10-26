{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    asksConfig,
    runApp
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
import Control.Monad.State.Lazy (MonadState (..), StateT, modify', runStateT)
import Data.Time.Clock (NominalDiffTime)
import Data.Time (getCurrentTime, diffUTCTime)
import Control.Monad.Random (StdGen, RandT, MonadRandom, mkStdGen, evalRandT)

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


-- |Tree of program paths. Each node is a statement, and its children
-- are next statements that can be executed. Exploring to a leaf node produces
-- a program path.
type PathTree = Tree Stmt

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
    , cfgSimplifierIntConstFolding :: !Bool
    , cfgSimplifierBoolConstFolding :: !Bool
    , cfgSimplifierChainAbsorb :: !Bool
    , cfgSimplifierChainDedup :: !Bool
    , cfgSimplifierChainTopBottom :: !Bool
    , cfgSimplifierChainComparison :: !Bool
    }
    deriving stock (Show, Eq)

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
runApp cfg (AppM m) = evalRandT (runStateT (runReaderT m cfg) emptyMetrics) (mkStdGen 42)

-- | Run the app and discard the metrics.
runApp_ :: Config -> AppM a -> IO a
runApp_ cfg app = fst <$> runApp cfg app

-- | Metrics produced by 'act', also folded into the enclosing total.
collectMetrics :: AppM a -> AppM (a, Metrics)
collectMetrics act = do
  before  <- get
  put mempty
  a       <- act
  scoped  <- get
  put (before <> scoped)
  pure (a, scoped)

data Metrics = Metrics
    { mtrFeasibilityChecks :: !Int
    , mtrFeasibilityTrivial :: !Int
    , mtrFeasibilityPruned :: !Int
    , mtrFeasibilityPrunedTrivial :: !Int
    , mtrSimplifierElapsed :: !NominalDiffTime
    , mtrElapsed  :: !NominalDiffTime
    }
    deriving stock (Show, Eq)

emptyMetrics :: Metrics
emptyMetrics = Metrics 0 0 0 0 0 0

-- Metrics form a monoid, so sub-computations can be measured and merged.
instance Semigroup Metrics where
    a <> b = Metrics
        { mtrFeasibilityChecks = mtrFeasibilityChecks a + mtrFeasibilityChecks b
        , mtrFeasibilityPruned   = mtrFeasibilityPruned a   + mtrFeasibilityPruned b
        , mtrFeasibilityTrivial    = mtrFeasibilityTrivial a    + mtrFeasibilityTrivial b
        , mtrFeasibilityPrunedTrivial  = mtrFeasibilityPrunedTrivial a  + mtrFeasibilityPrunedTrivial b
        , mtrElapsed = mtrElapsed a + mtrElapsed b
        , mtrSimplifierElapsed = mtrSimplifierElapsed a + mtrSimplifierElapsed b
        }

instance Monoid Metrics where
    mempty = emptyMetrics

timed :: AppM a -> AppM a
timed action = do
    t0 <- liftIO getCurrentTime
    r  <- action
    t1 <- liftIO getCurrentTime
    modify' $ \m ->
        m { mtrElapsed = mtrElapsed m + diffUTCTime t1 t0 }
    pure r
