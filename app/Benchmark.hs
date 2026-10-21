{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

import Criterion.Main
import qualified ShadowElimination as S
import SMT (getTypes)
import Unroll (unrollUntil)
import Feasibility (wlpAlwaysFeasibilityCheck)
import Optimize (optimize)
import SMT (z3Check)
import Control.Monad.Extra (firstJustM, when)
import GCLParser.GCLDatatype as G
import GCLParser.Parser (parseGCLfile)
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Maybe (fromJust, isNothing)
import Data.List (isPrefixOf)
import Data.Either (isLeft, fromRight)

-- The env function of criterion requires program to be NFData. We do not have an easy way to change the 
-- file program is defined in, so there is an instance declaration here.
instance Generic Program
instance NFData Program where rnf x = seq x ()

main :: IO ()
main = do 
  defaultMain [
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
    , benchPath "./examples/benchmark/min.gcl"             "min"
    , benchPath "./examples/benchmark/pullUp.gcl"          "pullUp"
    ]

-- | Runs the given program, with the given N value, on all settings
allSettings :: Program -> Int -> [Benchmark]
allSettings program n = [
  bench "default settings" $ nfIO $ run program n
  ]

-- | The default N values ran by all benchmarks
defaultNValues :: [Int]
defaultNValues = [1, 5, 10, 20]

-- | Benches the program at the path, while using the given name. Runs on all the defaultNValues on all settings
benchPath :: String -> String -> Benchmark
benchPath path name = env (return $ parseGCLfileWithMaybeN path ) $ \parseFunc ->
    benchProgram name parseFunc

-- | Benches on all defaultNValues
benchProgram :: String -> (Maybe Int -> IO (Program)) -> Benchmark
benchProgram name prog =  bgroup name $ 
  map (\n ->  env (prog (Just n)) $ \program -> 
    bgroup ("N = " ++ show n) $ allSettings program n) 
    defaultNValues

-- | Parses the gcl file, then declares N if it is supposed to be present. If a number is given, also gives N this value.
parseGCLfileWithMaybeN :: String -> (Maybe Int) -> IO (Program)
parseGCLfileWithMaybeN path nValue = do 
  maybeProgram <- parseGCLfile path
  when (isLeft maybeProgram) $ error "Program failed to parse"
  if shouldReplaceN path then do 
    let fixedProgram = parseGCLfileWithN' nValue maybeProgram
    return fixedProgram
  else do
    return (fromRight undefined maybeProgram)
  where
    shouldReplaceN path = isPrefixOf "./examples/benchmark" path

    parseGCLfileWithN' (Just n) (Right program)       = 
      program {input = (VarDeclaration "N" (PType PTInt)) : input program, stmt = Seq (Assign "N" (LitI n)) (stmt program)}
    parseGCLfileWithN' Nothing       (Right program)  =
      program {input = (VarDeclaration "N" (PType PTInt)) : input program}
    parseGCLfileWithN' _             (Left _)         = error $ path ++ " failed to parse"




run :: G.Program -> Int -> IO ()
run prog' k = do
    let (prog,(_, _, renamingRecovery)) = S.shadowElim prog'
    let paths = unrollUntil (fromIntegral k) $ G.stmt prog
    let varTypes = (getTypes prog)
    let exprs = concatMap (wlpAlwaysFeasibilityCheck varTypes []) paths
    let optimized = map optimize exprs
    maybeCounterExample <- firstJustM (z3Check varTypes . G.OpNeg) optimized
    return ()