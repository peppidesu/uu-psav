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
  statMetrics :: Metrics,
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
    (wasValid, !metrics) <- runBool prog cfg

    return $ Statistic {
        statName = name,
        statN = n,
        statK = cfgK cfg,
        statQ = cfgQ cfg,
        statFile = file,
        statMetrics = metrics,
        statWasValid = wasValid
    }



runBool :: G.Program -> Config -> IO ((Bool, Metrics))
runBool prog' config = runApp config $ do
    let (prog,(_, _, renamingRecovery)) = S.shadowElim prog'
    let paths = unrollUntil (cfgK config) $ G.stmt prog
    let varTypes = (getTypes prog)
    maybeCounterExample <- firstJustM (findCounterExample varTypes) paths
    return (isJust maybeCounterExample)
