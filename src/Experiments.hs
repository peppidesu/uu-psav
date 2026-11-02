module Experiments where
import Types
import Extra (listFiles, concatMapM)
import Data.List (sort, sortBy, sortOn)

data Experiment = Experiment String String Config Int

configSimplifierFull :: Experiment -> Experiment
configSimplifierFull (Experiment name file cfg n) = (Experiment (name ++ "_simplifier-full") file (cfg {
    cfgSimplifierEnabled=True,
    cfgSimplifierBoolConstFolding=True,
    cfgSimplifierChainTopBottom=True,
    cfgSimplifierChainDedup=True,
    cfgSimplifierChainComparison=True,
    cfgSimplifierChainAbsorb=True
}) n)

configSimplifierBasic :: Experiment -> Experiment
configSimplifierBasic (Experiment name file cfg n) = (Experiment (name ++ "_simplifier-basic") file (cfg {
    cfgSimplifierEnabled=True,
    cfgSimplifierBoolConstFolding=True,
    cfgSimplifierChainTopBottom=False,
    cfgSimplifierChainDedup=False,
    cfgSimplifierChainComparison=False,
    cfgSimplifierChainAbsorb=False
}) n)

configSimplifierNone :: Experiment -> Experiment
configSimplifierNone (Experiment name file cfg n) = (Experiment (name ++ "_simplifier-none") file (cfg {
    cfgSimplifierEnabled=False,
    cfgSimplifierBoolConstFolding=False,
    cfgSimplifierChainTopBottom=False,
    cfgSimplifierChainDedup=False,
    cfgSimplifierChainComparison=False,
    cfgSimplifierChainAbsorb=False
}) n)

configHeuristicOff :: Experiment -> Experiment
configHeuristicOff (Experiment name file cfg n) = (Experiment (name ++ "_heuristic-off") file (cfg {
    cfgHeuristic=False
}) n)

configHeuristicOn :: Experiment -> Experiment
configHeuristicOn (Experiment name file cfg n) = (Experiment (name ++ "_heuristic-on") file (cfg {
    cfgHeuristic=True,
    cfgAlwaysSimpleCheck=False
}) n)

configHeuristicOnBypass :: Experiment -> Experiment
configHeuristicOnBypass (Experiment name file cfg n) = (Experiment (name ++ "_heuristic-on-bypass") file (cfg {
    cfgHeuristic=True,
    cfgAlwaysSimpleCheck=True
}) n)


baseExperiment :: String -> String -> Experiment
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
withNKQ ns ks qs e
 = concatMap (\(Experiment name f cfg _) -> map (\n -> Experiment name f cfg n) ns)
 $ concatMap (\(Experiment name f cfg n) -> map (\k -> Experiment name f cfg { cfgK = k } n) ks)
 $ concatMap (\(Experiment name f cfg n) -> map (\q -> Experiment name f cfg { cfgQ = q } n) qs)
 $ [e]

performanceExperimentsSlow :: String -> [Int] -> Double -> [Experiment]
performanceExperimentsSlow file ks q = concat [
    (withNKQ [10] ks [q]) $ configHeuristicOff      $ configSimplifierBasic $ baseExperiment "perf" file,
    (withNKQ [10] ks [q]) $ configHeuristicOff      $ configSimplifierNone  $ baseExperiment "perf" file,
    (withNKQ [10] ks [q]) $ configHeuristicOn       $ configSimplifierNone  $ baseExperiment "perf" file
 ]
performanceExperimentsFast :: String -> [Int] -> Double -> [Experiment]
performanceExperimentsFast file ks q = concat [
    (withNKQ [10] ks [q]) $ configHeuristicOnBypass $ configSimplifierFull  $ baseExperiment "perf" file,
    (withNKQ [10] ks [q]) $ configHeuristicOn       $ configSimplifierFull  $ baseExperiment "perf" file,
    (withNKQ [10] ks [q]) $ configHeuristicOff      $ configSimplifierFull  $ baseExperiment "perf" file
 ]

performanceExperimentsQ :: String -> Int -> [Double] -> [Experiment]
performanceExperimentsQ file k qs = concat [
    (withNKQ [10] [k] qs) $ configHeuristicOnBypass $ configSimplifierFull  $ baseExperiment "perf" file
 ]

completenessExperiments :: String -> Int -> Double -> [Experiment]
completenessExperiments file k q = (withNKQ [2..10] [k] [q])
    $ configHeuristicOnBypass
    $ configSimplifierFull
    $ baseExperiment "perf" file

getMutantPaths :: IO [String]
getMutantPaths = concatMapM listFiles mutantDirectories

mutantDirectories :: [String]
mutantDirectories = map (\p -> "./examples/benchmark/mutants/" ++ p) [
    -- "bsort",
    "divByN",
    -- "find12",
    "memberOf",
    -- "min",
    "pullUp"]

experimentsMutants :: IO [Experiment]
experimentsMutants = do
    mutants <- getMutantPaths
    pure
        $ sortOn (\(Experiment _ _ _ n) -> n)
        $ concatMap (\f -> completenessExperiments f 50 1.0) mutants

performanceExperimentsAll :: [Experiment]
performanceExperimentsAll
    = concat [
        performanceExperimentsQ "./examples/benchmark/divByN.gcl"       40 [0, 1, 2, 4, 8, 16],
        performanceExperimentsQ "./examples/benchmark/memberOf.gcl"     40 [0, 1, 2, 4, 8, 16],
        performanceExperimentsQ "./examples/benchmark/pullUp.gcl"       40 [0, 1, 2, 4, 8, 16],

        performanceExperimentsFast "./examples/benchmark/divByN.gcl"    [k * 10 | k <- [1..4]]  16.0,
        performanceExperimentsFast "./examples/benchmark/memberOf.gcl"  [k * 10 | k <- [1..4]]  1.0,
        performanceExperimentsFast "./examples/benchmark/pullUp.gcl"    [k * 10 | k <- [1..4]]  1.0,

        performanceExperimentsSlow "./examples/benchmark/divByN.gcl"    [k * 10 | k <- [1..4]]  16.0,
        performanceExperimentsSlow "./examples/benchmark/memberOf.gcl"  [k * 10 | k <- [1..4]]  1.0,
        performanceExperimentsSlow "./examples/benchmark/pullUp.gcl"    [k * 10 | k <- [1..4]]  1.0
    ]
