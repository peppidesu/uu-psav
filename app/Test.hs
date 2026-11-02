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
import Experiments (experiments)

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
        "mtrFeasibilityChecks"              .= (mtrFeasibilityChecks              $ statMetrics s),
        "mtrFeasibilityZ3"                  .= (mtrFeasibilityZ3                  $ statMetrics s),
        "mtrFeasibilityTrivial"             .= (mtrFeasibilityTrivial             $ statMetrics s),
        "mtrFeasibilityPrunedZ3"            .= (mtrFeasibilityPrunedZ3            $ statMetrics s),
        "mtrFeasibilityPrunedTrivial"       .= (mtrFeasibilityPrunedTrivial       $ statMetrics s),
        "mtrFeasibilityPrunedAmountZ3"      .= (mtrFeasibilityPrunedAmountZ3      $ statMetrics s),
        "mtrFeasibilityPrunedAmountTrivial" .= (mtrFeasibilityPrunedAmountTrivial $ statMetrics s),
        "mtrFormulaLength"                  .= (mtrFormulaLength                  $ statMetrics s),
        "wasValid" .= (show $ statWasValid s)
     ]
instance DefaultOrdered Statistic where
    headerOrder _ = header [
        "name",
        "fileName",
        "N",
        "K",
        "Q",
        "mtrFeasibilityChecks",
        "mtrFeasibilityZ3",
        "mtrFeasibilityTrivial",
        "mtrFeasibilityPrunedZ3",
        "mtrFeasibilityPrunedTrivial",
        "mtrFeasibilityPrunedAmountZ3",
        "mtrFeasibilityPrunedAmountTrivial",
        "mtrFormulaLength",
        "wasValid"
     ]

<<<<<<< HEAD
=======
data Experiment = Experiment String String Config Int

nValues = [2..10]
qValues = [0.0, 1.0, 2.0, 4.0, 8.0, 16.0]

configSimplifierFull (Experiment name file cfg n) = (Experiment (name ++ "_simplifier-full") file (cfg {
    cfgSimplifierEnabled=True,
    cfgSimplifierBoolConstFolding=True,
    cfgSimplifierChainTopBottom=True,
    cfgSimplifierChainDedup=True,
    cfgSimplifierChainComparison=True,
    cfgSimplifierChainAbsorb=True
}) n)

configSimplifierBasic (Experiment name file cfg n) = (Experiment (name ++ "_simplifier-basic") file (cfg {
    cfgSimplifierEnabled=True,
    cfgSimplifierBoolConstFolding=True,
    cfgSimplifierChainTopBottom=False,
    cfgSimplifierChainDedup=False,
    cfgSimplifierChainComparison=False,
    cfgSimplifierChainAbsorb=False
}) n)

configSimplifierNone (Experiment name file cfg n) = (Experiment (name ++ "_simplifier-none") file (cfg {
    cfgSimplifierEnabled=False,
    cfgSimplifierBoolConstFolding=False,
    cfgSimplifierChainTopBottom=False,
    cfgSimplifierChainDedup=False,
    cfgSimplifierChainComparison=False,
    cfgSimplifierChainAbsorb=False
}) n)

configHeuristicOff (Experiment name file cfg n) = (Experiment (name ++ "_heuristic-off") file (cfg {
    cfgHeuristic=False
}) n)
configHeuristicOn (Experiment name file cfg n) = (Experiment (name ++ "_heuristic-on") file (cfg {
    cfgHeuristic=True,
    cfgAlwaysSimpleCheck=False
}) n)
configHeuristicOnBypass (Experiment name file cfg n) = (Experiment (name ++ "_heuristic-on-bypass") file (cfg {
    cfgHeuristic=True,
    cfgAlwaysSimpleCheck=True
}) n)


baseExperiment name file = Experiment name file (Config {
    cfgVerbose=False,
    cfgSimplifierEnabled=True,
    cfgSimplifierChainTopBottom=True,
    cfgSimplifierChainDedup=True,
    cfgSimplifierChainComparison=True,
    cfgSimplifierChainAbsorb=True,
    cfgSimplifierBoolConstFolding=True,
    cfgAlwaysSimpleCheck=True,
    cfgHeuristic=True,
    cfgK=0,
    cfgQ=1.0
 }) 10

withNKQ :: [Int] -> [Int] -> [Double] -> Experiment -> [Experiment]
withNKQ ns ks qs
 = concatMap (\(Experiment name f cfg _) -> map (\n -> Experiment name f cfg n) ns)
 . concatMap (\(Experiment name f cfg n) -> map (\k -> Experiment name f cfg { cfgK = k } n) ks)
 . concatMap (\(Experiment name f cfg n) -> map (\q -> Experiment name f cfg { cfgQ = q } n) qs)
 . pure

performanceExperimentsSlow :: String -> [Int] -> Double -> [Experiment]
performanceExperimentsSlow file ks q = concat [
    (withNKQ [10] ks [q]) $ configHeuristicOff      $ configSimplifierNone  $ baseExperiment "perf" file,
    (withNKQ [10] ks [q]) $ configHeuristicOff      $ configSimplifierBasic $ baseExperiment "perf" file,
    (withNKQ [10] ks [q]) $ configHeuristicOn       $ configSimplifierNone  $ baseExperiment "perf" file
 ]
performanceExperimentsFast :: String -> [Int] -> Double -> [Experiment]
performanceExperimentsFast file ks q = concat [
    (withNKQ [10] ks [q]) $ configHeuristicOff      $ configSimplifierFull  $ baseExperiment "perf" file,
    (withNKQ [10] ks [q]) $ configHeuristicOn       $ configSimplifierFull  $ baseExperiment "perf" file,
    (withNKQ [10] ks [q]) $ configHeuristicOnBypass $ configSimplifierFull  $ baseExperiment "perf" file
 ]

performanceExperimentsQ :: String -> Int -> [Double] -> [Experiment]
performanceExperimentsQ file k qs = concat [
    (withNKQ [10] [k] qs) $ configHeuristicOff      $ configSimplifierFull  $ baseExperiment "perf" file,
    (withNKQ [10] [k] qs) $ configHeuristicOn       $ configSimplifierFull  $ baseExperiment "perf" file,
    (withNKQ [10] [k] qs) $ configHeuristicOnBypass $ configSimplifierFull  $ baseExperiment "perf" file
 ]

completenessExperiments file k q = (withNKQ [2..10] [k] [q])
    $ configHeuristicOnBypass
    $ configSimplifierFull
    $ baseExperiment "perf" file


experiments = concat [
    completenessExperiments "./examples/benchmark/invalidDivByN.gcl"    70 2.0,
    completenessExperiments "./examples/benchmark/invalidMemberOf.gcl"  70 2.0,
    completenessExperiments "./examples/benchmark/invalidPullUp.gcl"    70 2.0,

    performanceExperimentsSlow "./examples/benchmark/invalidDivByN.gcl"    [k * 10 | k <- [1..8]]  2.0,
    performanceExperimentsSlow "./examples/benchmark/invalidMemberOf.gcl"  [k * 10 | k <- [1..5]]  2.0,
    performanceExperimentsSlow "./examples/benchmark/invalidPullUp.gcl"    [k * 10 | k <- [1..5]]  2.0,

    performanceExperimentsFast "./examples/benchmark/invalidDivByN.gcl"    [k * 10 | k <- [1..10]] 2.0,
    performanceExperimentsFast "./examples/benchmark/invalidMemberOf.gcl"  [k * 10 | k <- [1..7]]  2.0,
    performanceExperimentsFast "./examples/benchmark/invalidPullUp.gcl"    [k * 10 | k <- [1..7]]  2.0,

    performanceExperimentsQ "./examples/benchmark/invalidDivByN.gcl"       50 [-1, 0, 1, 2, 4, 8, 16],
    performanceExperimentsQ "./examples/benchmark/invalidMemberOf.gcl"     50 [-1, 0, 1, 2, 4, 8, 16],
    performanceExperimentsQ "./examples/benchmark/invalidPullUp.gcl"       50 [-1, 0, 1, 2, 4, 8, 16]
 ]

>>>>>>> 52c9b2a (more statistics)
generateStatistics :: IO ()
generateStatistics = do
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
