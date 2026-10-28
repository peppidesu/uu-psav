{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

import Criterion.Main
import qualified ShadowElimination as S
import SMT (getTypes)
import Unroll (unrollUntil)
import Feasibility (findCounterExample)
import Optimize (optimize)
import SMT (z3Check)
import Control.Monad.Extra (firstJustM, when, concatMapM)
import GCLParser.GCLDatatype as G
import GCLParser.Parser (parseGCLfile)
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Maybe (fromJust, isNothing)
import Data.List (isPrefixOf, stripPrefix)
import Data.Either (isLeft, fromRight)
import Types
import Control.Monad.IO.Class
import Parse (parseGCLfileWithMaybeN)
import System.Directory.Extra (listFiles)

-- The env function of criterion requires program to be NFData. We do not have an easy way to change the
-- file program is defined in, so there is an instance declaration here.
instance Generic Program
instance NFData Program where rnf x = seq x ()

main :: IO ()
main = do
  mutants <- getMutantPaths

  defaultMain [
    bgroup "regular"[
      benchPath "./examples/benchmark/bsort.gcl"           "bsort"
    , benchPath "./examples/benchmark/divByN.gcl"          "divByN"
    -- , benchPath "./examples/benchmark/find12.gcl"          "find12"
    , benchPath "./examples/benchmark/invalidBsort.gcl"    "invalidBsort"
    , benchPath "./examples/benchmark/invalidDivByN.gcl"   "invalidDivByN"
    -- , benchPath "./examples/benchmark/invalidFind12.gcl"   "invalidFind12"
    , benchPath "./examples/benchmark/invalidMemberOf.gcl" "invalidMemberOf"
    -- , benchPath "./examples/benchmark/invalidMin.gcl"      "invalidMin"
    , benchPath "./examples/benchmark/invalidPullUp.gcl"   "invalidPullUp"
    , benchPath "./examples/benchmark/memberOf.gcl"        "memberOf"
    -- , benchPath "./examples/benchmark/min.gcl"             "min"
    , benchPath "./examples/benchmark/pullUp.gcl"          "pullUp"
    ],
    bgroup "mutants" (map benchDirectory mutants)
    ]

-- | Gets a list of tuples, with (mutantDirectory, [MutantPaths])
getMutantPaths :: IO ([(String, [String])])
getMutantPaths =  do
  pss <- mapM listFiles mutantDirectories
  return $ zip mutantDirectories pss
    where 
      mutantDirectories = map (\p -> "./examples/benchmark/mutants/" ++ p) [
        "divByN",
        "memberOf",
        "pullUp"]

-- | Runs the given program, with the given N value, on all settings
allSettings :: Program -> Int -> [Benchmark]
allSettings program k = [
  bench "default settings" $ nfIO $ run program k (Config {
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
  ]

bestSetting ::Program -> Int -> Benchmark
bestSetting program k =
  bench "best setting" $ nfIO $ run program k (Config {
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

defaultNValues :: [Int]
defaultNValues = [2, 3, 4, 5, 6, 7, 8, 9, 10]

defaultNValue :: Int
defaultNValue = 10

defaultKValues :: [Int]
defaultKValues = [8, 16, 32, 64]

defaultKValue :: Int
defaultKValue = 4

-- | Benches the directory of mutants. This function should be given the path of the directory, and a list of paths to files within that directory.
benchDirectory :: (String, [String]) -> Benchmark
benchDirectory (directory, paths) = bgroup (getName directory) (map (\p -> benchMutantPath p (getName p)) paths)
  where 
    getName path = fromJust $ stripPrefix "./examples/benchmark/mutants" path

-- | Benches the mutants as needed in the second experiment
benchMutantPath :: String -> String -> Benchmark
benchMutantPath path name = env (return $ parseGCLfileWithMaybeN path ) $ \parseFunc ->
    benchMutantProgram name parseFunc

-- | Benches on the default k values, with all the n values
benchMutantProgram :: String -> (Maybe Int -> IO (Program)) -> Benchmark
benchMutantProgram name prog =  bgroup name $
  map (\n -> mkGroup defaultKValue n) defaultNValues
  where
      mkGroup k n = env (prog (Just n)) $ \program ->
          bgroup ("N = " ++ show n ++ ", K = " ++ show k) $ [bestSetting program k]

-- | Benches the program at the path, while using the given name, as needed with the first experiment
benchPath :: String -> String -> Benchmark
benchPath path name = env (return $ parseGCLfileWithMaybeN path ) $ \parseFunc ->
    benchProgram name parseFunc

-- | Benches on all default k values, with the single n value
benchProgram :: String -> (Maybe Int -> IO (Program)) -> Benchmark
benchProgram name prog =  bgroup name $
  map (\k -> mkGroup k defaultNValue) defaultKValues
  where
      mkGroup k n = env (prog (Just n)) $ \program ->
          bgroup ("N = " ++ show n ++ ", K = " ++ show k) $ allSettings program k

run :: G.Program -> Int -> Config -> IO ()
run prog' k config = do
    (result, _metrics) <- runApp config $ run'
    pure $ result
  where
    run' = do
      let (prog,(_, _, _renamingRecovery)) = S.shadowElim prog'
      let paths = unrollUntil (fromIntegral k) $ G.stmt prog
      let varTypes = (getTypes prog)
      _ <- firstJustM (findCounterExample varTypes) paths
      return ()
