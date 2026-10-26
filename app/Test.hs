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
import Optimize
import Control.Monad.IO.Class (MonadIO(liftIO))
import Parse (parseGCLfileWithMaybeN)
import System.Directory.Extra


main :: IO ()
main = do
  testPathsAndName <- getTestPathsPerDir
  let testTrees = map createTestTree testPathsAndName
  defaultMain $ testGroup "Tests" testTrees

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
