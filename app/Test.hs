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

bigFunc :: IO ()
bigFunc = do
  testPaths <- getTestPaths
  generateStatisticsFile testPaths
  where
    getTestPaths :: IO ([String])
    getTestPaths = do
      mutants <- concatMapM listFiles mutantDirectories
      -- TODO: add more
      return mutants


generateStatisticsFile :: [String] -> IO ()
generateStatisticsFile paths = do
  statistics <- collectStatistics paths
  let saveLocation = "./test.csv"
  createStatisticFile saveLocation statistics

-- Statistics collection

collectStatistics :: [String] -> IO ([Statistic])
collectStatistics paths = concatMapM collectStatisticsWithPath paths

collectStatisticsWithPath :: String -> IO ([Statistic])
collectStatisticsWithPath path = do mapM (collectStatistic' path) defaultToTest
  where 
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
allNValues = [2, 5, 10]

allConfigs :: [Config]
allConfigs = [Config {
         cfgVerbose=False,
         cfgSimplifierIntConstFolding=True,
         cfgSimplifierEnabled=True,
         cfgSimplifierChainTopBottom=True,
         cfgSimplifierChainDedup=True,
         cfgSimplifierChainComparison=True,
         cfgSimplifierChainAbsorb=True,
         cfgSimplifierBoolConstFolding=True,
         cfgK=30
     },
     Config {
         cfgVerbose=False,
         cfgSimplifierIntConstFolding=True,
         cfgSimplifierEnabled=False,
         cfgSimplifierChainTopBottom=True,
         cfgSimplifierChainDedup=True,
         cfgSimplifierChainComparison=True,
         cfgSimplifierChainAbsorb=True,
         cfgSimplifierBoolConstFolding=True,
         cfgK=30
     }]

defaultToTest :: [(Int, Config)]
defaultToTest = [ (y,x) | x<-allConfigs, y<-allNValues ]

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
    testCase ("simplifier")    (testFunc p n (configFullSimplifier 70) @? "Mutant not killed"),
    testCase ("no simplifier") (testFunc p n (configNoSimplifier 70)   @? "Mutant not killed")]) toTest
  where
    getTestName path = last $ splitOn "/" path
    testFunc p n config = do
      prog <- parseGCLfileWithMaybeN p (Just n)
      runBool prog (cfgK config) config

getTestPathsPerDir :: IO [(String,[(String, Int)])]
getTestPathsPerDir = do
  pss <- mapM listFiles mutantDirectories
  return $ zip mutantDirectories (map (\ps -> map (\p -> (p, 4)) ps) pss)

mutantDirectories :: [String]
mutantDirectories = map (\p -> "./examples/benchmark/mutants/" ++ p) [
  "bsort",
  "divByN",
  -- "find12",
  "memberOf",
  -- "min",
  "pullUp"]

runBool :: G.Program -> Int -> Config -> IO (Bool)
runBool prog' k config = do
    (result, metrics) <- runApp config $ run'
    liftIO $ putStrLn $ show metrics
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
         cfgK=k
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
         cfgK=k
     })
