{-# LANGUAGE LambdaCase, OverloadedRecordDot #-}

-- | Module that parses the arguments given to the executable

module CLI (
  Options, CLIGeneric(..), Inputs, PlainData
, createSettings
, getOpts
, getInputs
, getPlain
, usage
)

where

import System.Environment
import System.Console.GetOpt
import GCLParser.Parser (parseGCLfile, ParseResult)
import GCLParser.GCLDatatype 
import Data.List.Extra (mconcatMap)
import qualified Z3Conf  
import qualified Z3.Opts as Z3 

type PathLength = Int
type UnsatKind = Bool

data UnsatFreq = UnsatFreq UnsatKind Int
   deriving (Show, Eq)


-- CLIGeneric is the general datatype for CLI processed data
-- conf is meant to store the configuration script parametners
-- gclPaths is meant to store the GCL programs 
data CLIGeneric a b c = CLI
  { plength   :: a
  , conf  :: b
  , gcl  :: [c]
  , showHelp :: Bool
  }
  deriving (Show, Eq)

-- conf: stores the location of the config file if the user gave a path otherwise Nothing
-- gcl:  stores in a list the paths to the GCL program files
type Options = CLIGeneric (Maybe PathLength) (Maybe String) String

defaultOptions :: Options
defaultOptions = CLI
   { plength  = Nothing
   , conf = Nothing
   , gcl = []
   , showHelp = False
   }

appendGCLFile :: String -> Options -> Options
appendGCLFile mStr opts = opts {gcl = mStr: opts.gcl}

reverseGCLFiles :: Options -> Options
reverseGCLFiles opts = opts {gcl = reverse $ opts.gcl}

options :: [OptDescr (Options ->  Options)]
options =
  [ Option "K" []
         (ReqArg (\str opts -> opts {plength = Just (read str :: Int)}) "<Max path length>")
         "The upper length limit K on the verification paths to be checked"
--         "Check for unsatisfiability every N times, where N is the parameter"
--         "Unsatisfiability frequency K/N where N is the parameter"
  , Option "f" ["file"]
         (ReqArg appendGCLFile  "<GCL program filepath>")
         "Thet path to a GCL program File"
  , Option "c" ["conf"]
         (ReqArg (\str opts -> opts {conf = Just str}) "<Z3 conf filepath>")
         "A path to a configuration file for the z3 backend in key-value format"
  , Option "h" ["help"]
         (NoArg (\opts -> opts {showHelp = True}))
         "Print the CLI usage, ignoring all other arguments"
  ]

-- | The thing to print when the --help flag shows or if the arguments given are wrong.
usage :: String
usage = usageInfo "Usage: <name-of>executable> -K NUMBER [OPTION...] files..." options


createSettings :: [String] -> IO Options
createSettings argv =
     case getOpt (ReturnInOrder appendGCLFile)  options argv of
         (o,[],[] ) -> return $ foldl (flip id) defaultOptions o
         (_,_,errs) -> ioError (userError (concat errs ++ usage))


-- | Get the configuration parameters
-- 
getOpts :: IO Options
getOpts = getArgs >>= fmap reverseGCLFiles . createSettings



type PlainData = CLIGeneric PathLength [(String,String)] (ParseResult Program)



-- conf: Stores the parsed conf file key-value pairs into a term of type Z3.Opts
-- gcl:  Stores the parsed programs given by file paths from the user
--       including the case that a parsed file may have an error

-- | Read the arguments given to the executable, process them and return the results
-- Prefer this function if you desire to print the resulting processing.
-- This is because Z3.Opts datatype has no instance of Show
getPlain :: IO PlainData
getPlain = do
        opts <- getOpts
        let progPaths = opts.gcl
            confPath  = opts.conf
            numKMaybe = opts.plength
            showHelp'  = opts.showHelp
            parseGCLFiles = mapM parseGCLfile progPaths
            
         in do progs <- parseGCLFiles
               plainConf <- Z3Conf.getPlain confPath
               case numKMaybe of
                 Nothing -> ioError $ userError $ "\nError: Flag -K is required\n" ++ usage 
                 Just k -> return CLI {
                   plength  = k,
                   conf     = plainConf,
                   gcl      = progs,
                   showHelp = showHelp'
                   }

type Inputs = CLIGeneric PathLength Z3.Opts (ParseResult Program)

plainToZ3Opt :: PlainData -> Inputs
plainToZ3Opt plain = plain {conf =  mconcatMap (uncurry Z3.opt) plain.conf}

getInputs :: IO Inputs
getInputs =  plainToZ3Opt <$> getPlain
