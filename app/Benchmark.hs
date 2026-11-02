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
import GCLParser.GCLDatatype as G ( Program(stmt) )
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
import Experiments 
import Criterion.Types (Benchmark(Benchmark))

-- The env function of criterion requires program to be NFData. We do not have an easy way to change the
-- file program is defined in, so there is an instance declaration here.
instance Generic Program
instance NFData Program where rnf x = seq x ()

main :: IO ()
main = do
  secondExperiments <- experimentsMutants
  defaultMain [
    bgroup "experiment=1" (makeBenchMarks performanceExperimentsAll 1)
    -- , bgroup "experiment=2" (makeBenchMarks secondExperiments 2)
    ]

makeBenchMarks :: [Experiment] -> Int -> [Benchmark]
makeBenchMarks experiments expNum = map (make expNum) experiments
  where 
    make :: Int -> Experiment -> Benchmark
    make expNum (Experiment name file cfg n) = env (parseGCLfileWithMaybeN file (Just n)) $ \program ->
      bench (createName name file cfg n expNum) $ nfIO $ run program (cfgK cfg) cfg
    createName name file cfg n expNum = "Name=" ++ name ++ ",File=" ++ file ++ ",N=" ++ show n ++ ",K=" ++ show (cfgK cfg) ++ ",Q=" ++ show (cfgQ cfg) ++ ",Experiment=" ++ show expNum

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
