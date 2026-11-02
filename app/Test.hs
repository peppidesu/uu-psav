{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
import Test.Tasty
import Test.Tasty.HUnit
import Data.List.Extra (splitOn)
import Types
import qualified GCLParser.GCLDatatype as G
import Data.Maybe (isJust)
import qualified ShadowElimination as S
import Unroll
import SMT (getTypes)
import Feasibility
import Control.Monad.Extra (firstJustM)
import Parse (parseGCLfileWithMaybeN)
import System.Directory.Extra
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (DefaultOrdered (..), ToNamedRecord (..), header, (.=), namedRecord, encodeDefaultOrderedByName)
import Experiments (Experiment(..))
import Experiments (performanceExperimentsAll, experimentsMutants)

main :: IO ()
main = do
    generateStatistics
    -- testPathsAndName <- getTestPathsPerDir
    -- let testTrees = map createTestTree testPathsAndName
    -- defaultMain $ testGroup "Tests" testTrees


data Statistic = Statistic {
  statName :: String,
  statN :: Int,
  statK :: Int,
  statQ :: Double,
  statMetricsMean :: MetricsAvStd,
  statMetricsStd :: MetricsAvStd,
  statFile :: String,
  statWasValid :: Bool
} deriving (Generic, Show)


instance ToNamedRecord Statistic where
    toNamedRecord s = namedRecord [
        "name" .= statName s,
        "fileName" .= statFile s,
        "N" .= statN s,
        "K" .= statK s,
        "Q" .= statQ s,
        "mtrMeanFeasibilityChecks"              .= (mtrAvStdFeasibilityChecks              $ statMetricsMean s),
        "mtrMeanFeasibilityZ3"                  .= (mtrAvStdFeasibilityZ3                  $ statMetricsMean s),
        "mtrMeanFeasibilityTrivial"             .= (mtrAvStdFeasibilityTrivial             $ statMetricsMean s),
        "mtrMeanFeasibilityPrunedZ3"            .= (mtrAvStdFeasibilityPrunedZ3            $ statMetricsMean s),
        "mtrMeanFeasibilityPrunedTrivial"       .= (mtrAvStdFeasibilityPrunedTrivial       $ statMetricsMean s),
        "mtrMeanFeasibilityPrunedAmountZ3"      .= (mtrAvStdFeasibilityPrunedAmountZ3      $ statMetricsMean s),
        "mtrMeanFeasibilityPrunedAmountTrivial" .= (mtrAvStdFeasibilityPrunedAmountTrivial $ statMetricsMean s),
        "mtrMeanFormulaLength"                  .= (mtrAvStdFormulaLength                  $ statMetricsMean s),

        "mtrStdFeasibilityChecks"              .= (mtrAvStdFeasibilityChecks              $ statMetricsStd s),
        "mtrStdFeasibilityZ3"                  .= (mtrAvStdFeasibilityZ3                  $ statMetricsStd s),
        "mtrStdFeasibilityTrivial"             .= (mtrAvStdFeasibilityTrivial             $ statMetricsStd s),
        "mtrStdFeasibilityPrunedZ3"            .= (mtrAvStdFeasibilityPrunedZ3            $ statMetricsStd s),
        "mtrStdFeasibilityPrunedTrivial"       .= (mtrAvStdFeasibilityPrunedTrivial       $ statMetricsStd s),
        "mtrStdFeasibilityPrunedAmountZ3"      .= (mtrAvStdFeasibilityPrunedAmountZ3      $ statMetricsStd s),
        "mtrStdFeasibilityPrunedAmountTrivial" .= (mtrAvStdFeasibilityPrunedAmountTrivial $ statMetricsStd s),

        "mtrAvStdPathsInspected"               .= (mtrAvStdPathsInspected                 $ statMetricsStd s),
        "mtrStdFormulaLength"                  .= (mtrAvStdFormulaLength                  $ statMetricsStd s),
        "wasValid" .= (show $ statWasValid s)
     ]

instance DefaultOrdered Statistic where
    headerOrder _ = header [
        "name",
        "fileName",
        "N",
        "K",
        "Q",
        "mtrMeanFeasibilityChecks",
        "mtrMeanFeasibilityZ3",
        "mtrMeanFeasibilityTrivial",
        "mtrMeanFeasibilityPrunedZ3",
        "mtrMeanFeasibilityPrunedTrivial",
        "mtrMeanFeasibilityPrunedAmountZ3",
        "mtrMeanFeasibilityPrunedAmountTrivial",
        "mtrMeanFormulaLength",
        "mtrStdFeasibilityChecks",
        "mtrStdFeasibilityZ3",
        "mtrStdFeasibilityTrivial",
        "mtrStdFeasibilityPrunedZ3",
        "mtrStdFeasibilityPrunedTrivial",
        "mtrStdFeasibilityPrunedAmountZ3",
        "mtrStdFeasibilityPrunedAmountTrivial",
        "mtrStdFormulaLength",
        "wasValid"
     ]

generateStatistics :: IO ()
generateStatistics = do
    -- let experiments = performanceExperimentsAll
    experiments <- experimentsMutants

    statistics <- mapM runExperiment experiments
    putStrLn "writing file..."
    BL.writeFile "./stats.csv" $ encodeDefaultOrderedByName statistics

-- Tests

createTestTree :: (String, [(String, Int)]) -> TestTree
createTestTree (name, toTest) =
  testGroup name $
  map (\(p, n) -> testGroup (getTestName p) [
    testCase ("simplifier") (testFunc p n (Config {
        cfgVerbose=False,
        cfgSimplifierEnabled=True,
        cfgSimplifierChainTopBottom=True,
        cfgSimplifierChainDedup=True,
        cfgSimplifierChainComparison=True,
        cfgSimplifierChainAbsorb=True,
        cfgSimplifierBoolConstFolding=True,
        cfgAlwaysSimpleCheck=True,
        cfgHeuristic=True,
        cfgK=80,
        cfgQ=1
    }) @? "Mutant not killed"),
    testCase ("no simplifier") (testFunc p n (Config {
        cfgVerbose=False,
        cfgSimplifierEnabled=True,
        cfgSimplifierChainTopBottom=True,
        cfgSimplifierChainDedup=True,
        cfgSimplifierChainComparison=True,
        cfgSimplifierChainAbsorb=True,
        cfgSimplifierBoolConstFolding=True,
        cfgAlwaysSimpleCheck=True,
        cfgHeuristic=True,
        cfgK=80,
        cfgQ=1
    }) @? "Mutant not killed")]) toTest
  where
    getTestName path = last $ splitOn "/" path
    testFunc p n config = do
      prog <- parseGCLfileWithMaybeN p (Just n)
      fst <$> runBool prog config

getTestPathsPerDir :: IO [(String,[(String, Int)])]
getTestPathsPerDir = do
  pss <- mapM listFiles mutantDirectories
  return $ zip mutantDirectories (map (\ps -> map (\p -> (p, 6)) ps) pss)

mutantDirectories :: [String]
mutantDirectories = map (\p -> "./examples/benchmark/mutants/" ++ p) [
  "divByN",
  "memberOf",
  "pullUp"]

runExperiment :: Experiment -> IO (Statistic)
runExperiment (Experiment name file cfg n) = do
    putStrLn $ "Running experiment:" ++ name ++ ['\n']
        ++ "  file: " ++ file ++ ['\n']
        ++ "  n = " ++ show n ++ ", k = " ++ show (cfgK cfg) ++ ", q = " ++ show (cfgQ cfg)
    prog <- parseGCLfileWithMaybeN file (Just n)
    results <- mapM (const $ runBool prog cfg) [1..10]
    let (wasValid, metrics) = unzip results
    let (metricsMean, metricsStd) = toMetricsAvStd $ metrics

    return $ Statistic {
        statName = name,
        statN = n,
        statK = cfgK cfg,
        statQ = cfgQ cfg,
        statFile = file,
        statMetricsMean = metricsMean,
        statMetricsStd = metricsStd,
        statWasValid = or wasValid
    }

runBool :: G.Program -> Config -> IO ((Bool, Metrics))
runBool prog' config = runApp config $ do
    let (prog,(_, _, renamingRecovery)) = S.shadowElim prog'
    let paths = unrollUntil (cfgK config) $ G.stmt prog
    let varTypes = (getTypes prog)
    maybeCounterExample <- firstJustM (findCounterExample varTypes) paths
    return (isJust maybeCounterExample)
