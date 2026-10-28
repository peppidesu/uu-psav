{-# LANGUAGE DeriveGeneric #-}
import Test.Tasty
import Test.Tasty.HUnit
import Data.List.Extra (splitOn)
import Types
import GCLParser.GCLDatatype as G
import Data.Maybe (isNothing, isJust)
import qualified ShadowElimination as S
import Unroll
import SMT (getTypes, z3Check)
import Feasibility
import Control.Monad.Extra (concatMapM, firstJustM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Parse (parseGCLfileWithMaybeN)
import System.Directory.Extra
import GHC.Generics (Generic)


main :: IO ()
main = do
  bigFunc
  -- testPathsAndName <- getTestPathsPerDir
  -- let testTrees = map createTestTree testPathsAndName
  -- defaultMain $ testGroup "Tests" testTrees

data Statistic = Statistic {
  config :: Config,
  metrics :: Metrics,
  fileName :: String,
  nValue :: Int,
  wasValid :: Bool
} deriving (Generic, Show)

data RunMethod = FirstExperiment | SecondExperiment

bigFunc :: IO ()
bigFunc = do
  let benchTestPaths = ["./examples/benchmark/divByN.gcl", "./examples/benchmark/invalidDivByN.gcl", "./examples/benchmark/invalidMemberOf.gcl", "./examples/benchmark/invalidPullUp.gcl", "./examples/benchmark/memberOf.gcl", "./examples/benchmark/pullUp.gcl"]
  generateStatisticsFile FirstExperiment "./statsExperiment1.csv" benchTestPaths
  mutantTestPaths <- getMutantTestPaths
  generateStatisticsFile SecondExperiment "./statsExperiment2.csv" mutantTestPaths
  where
    getMutantTestPaths :: IO ([String])
    getMutantTestPaths = do
      mutants <- concatMapM listFiles mutantDirectories
      -- TODO: add more
      return mutants


generateStatisticsFile :: RunMethod -> String -> [String] -> IO ()
generateStatisticsFile runMeth path paths = do
  statistics <- collectStatistics runMeth paths
  let saveLocation = path
  createStatisticFile saveLocation statistics

-- Statistics collection

collectStatistics :: RunMethod ->[String] -> IO ([Statistic])
collectStatistics runMeth paths = concatMapM (collectStatisticsWithPath runMeth) paths

collectStatisticsWithPath :: RunMethod -> String -> IO ([Statistic])
collectStatisticsWithPath FirstExperiment  path = do mapM (collectStatistic' path) benchToTest
collectStatisticsWithPath SecondExperiment path = do mapM (collectStatistic' path) mutantToTest

collectStatistic' path (n, config) = do
  program <- parseGCLfileWithMaybeN path (Just $ n)
  (metric, runResult) <- runCollectMetricAndBool program (cfgK config) config
  return Statistic{config = config, metrics = metric, fileName = path, nValue = n, wasValid = runResult }

runCollectMetricAndBool :: G.Program -> Int -> Config -> IO ((Metrics, Bool))
runCollectMetricAndBool prog' k config = do
    (result, metrics) <- runApp config $ run'
    return (metrics, result)
  where
    run' = do
      let (prog,(_, _, renamingRecovery)) = S.shadowElim prog'
      let paths = unrollUntil (fromIntegral k) $ G.stmt prog
      let varTypes = (getTypes prog)
      maybeCounterExample <- firstJustM (findCounterExample varTypes) paths
      return (isNothing maybeCounterExample)

-- Configuration

allNValues :: [Int]
allNValues = [2, 3, 4, 5, 6, 7, 8, 9, 10]

allKValues = [8, 16, 32, 64]

allConfigs :: Int -> [Config]
allConfigs k = [Config {
         cfgVerbose=False,
         cfgSimplifierIntConstFolding=True,
         cfgSimplifierEnabled=True,
         cfgSimplifierChainTopBottom=True,
         cfgSimplifierChainDedup=True,
         cfgSimplifierChainComparison=True,
         cfgSimplifierChainAbsorb=True,
         cfgSimplifierBoolConstFolding=True,
         cfgK=k,
         cfgQ=1
     }]

bestConfig :: Config
-- TODO: replace with our best config
bestConfig = error "add best config to code or comment out the running of the mutants"

mutantToTest :: [(Int, Config)]
mutantToTest = [(x, bestConfig)| x <- allNValues]

benchToTest :: [(Int, Config)]
benchToTest = [(10, c)| c <- concatMap allConfigs allKValues]

-- defaultToTest :: [(Int, Config)]
-- defaultToTest = [ (y,x) | x<-allConfigs, y<-allNValues ]

-- To csv
createStatisticFile :: String -> [Statistic] -> IO ()
createStatisticFile path statistics = do
  let csvString = unlines (csvHeader : (map createCsvLine statistics))
  writeFile path csvString

createCsvLine :: Statistic -> String
createCsvLine statistic =
  createConfigPart statistic    ++ ","
  ++ createMetricPart statistic ++ ","
  ++ fileName statistic         ++ ","
  ++ show (nValue statistic)    ++ ","
  ++ show (wasValid statistic)
  where
  createConfigPart realStatistic = let statistic = config realStatistic in
       show (cfgVerbose statistic)                    ++ ","
    ++ show (cfgSimplifierEnabled statistic)          ++ ","
    ++ show (cfgSimplifierIntConstFolding statistic)  ++ ","
    ++ show (cfgSimplifierBoolConstFolding statistic) ++ ","
    ++ show (cfgSimplifierChainAbsorb statistic)      ++ ","
    ++ show (cfgSimplifierChainDedup statistic)       ++ ","
    ++ show (cfgSimplifierChainTopBottom statistic)   ++ ","
    ++ show (cfgSimplifierChainComparison statistic)  ++ ","
    ++ show (cfgK statistic)
  createMetricPart statistic = let metric = metrics statistic in
       show (mtrFeasibilityChecks metric)        ++ ","
    ++ show (mtrFeasibilityTrivial metric)       ++ ","
    ++ show (mtrFeasibilityPruned metric)        ++ ","
    ++ show (mtrFeasibilityPrunedTrivial metric) ++ ","

csvHeader :: String
csvHeader = configcsvHeader ++ "," ++ metricscsvHeader ++ "," ++"fileName,nValue,wasValid"
  where
    configcsvHeader = "cfgVerbose,cfgSimplifierEnabled,cfgSimplifierIntConstFolding,cfgSimplifierBoolConstFolding,cfgSimplifierChainAbsorb,cfgSimplifierChainDedup,cfgSimplifierChainTopBottom,cfgSimplifierChainComparison,cfgK"
    metricscsvHeader = "mtrFeasibilityChecks,mtrFeasibilityTrivial,mtrFeasibilityPruned,mtrFeasibilityPrunedTrivial"

-- Tests

createTestTree :: (String, [(String, Int)]) -> TestTree
createTestTree (name, toTest) =
  testGroup name $
  map (\(p, n) -> testGroup (getTestName p) [
    testCase ("simplifier")    (testFunc p n (configFullSimplifier 80) @? "Mutant not killed"),
    testCase ("no simplifier") (testFunc p n (configNoSimplifier 80)   @? "Mutant not killed")]) toTest
  where
    getTestName path = last $ splitOn "/" path
    testFunc p n config = do
      prog <- parseGCLfileWithMaybeN p (Just n)
      runBool prog (cfgK config) config

getTestPathsPerDir :: IO [(String,[(String, Int)])]
getTestPathsPerDir = do
  pss <- mapM listFiles mutantDirectories
  return $ zip mutantDirectories (map (\ps -> map (\p -> (p, 6)) ps) pss)

mutantDirectories :: [String]
mutantDirectories = map (\p -> "./examples/benchmark/mutants/" ++ p) [
  "divByN",
  -- "find12",
  "memberOf",
  -- "min",
  "pullUp"]

runBool :: G.Program -> Int -> Config -> IO (Bool)
runBool prog' k config = do
    (result, metrics) <- runApp config $ run'
    putStrLn $ show metrics
    pure $ result
  where
    run' = do
      let (prog,(_, _, renamingRecovery)) = S.shadowElim prog'
      let paths = unrollUntil (fromIntegral k) $ G.stmt prog
      let varTypes = (getTypes prog)
      maybeCounterExample <- firstJustM (findCounterExample varTypes) paths
      return (isJust maybeCounterExample)

configNoSimplifier k = (Config {
         cfgVerbose=False,
         cfgSimplifierIntConstFolding=True,
         cfgSimplifierEnabled=False,
         cfgSimplifierChainTopBottom=True,
         cfgSimplifierChainDedup=True,
         cfgSimplifierChainComparison=True,
         cfgSimplifierChainAbsorb=True,
         cfgSimplifierBoolConstFolding=True,
         cfgK=k,
         cfgQ=1
     })

configFullSimplifier k = (Config {
         cfgVerbose=False,
         cfgSimplifierIntConstFolding=True,
         cfgSimplifierEnabled=True,
         cfgSimplifierChainTopBottom=True,
         cfgSimplifierChainDedup=True,
         cfgSimplifierChainComparison=True,
         cfgSimplifierChainAbsorb=True,
         cfgSimplifierBoolConstFolding=True,
         cfgK=k,
         cfgQ=1
     })
