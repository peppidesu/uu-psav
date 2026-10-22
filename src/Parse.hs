module Parse where
import GCLParser.GCLDatatype as G
import GCLParser.Parser (parseGCLfile, ParseResult)
import Control.Monad (when)
import Data.Either
import Data.List (isPrefixOf)

parseGCLfileWithMaybeN' :: String -> (Maybe Int) -> IO (ParseResult Program)
parseGCLfileWithMaybeN' path nValue = do
   a <- (parseGCLfileWithMaybeN path nValue)
   return $ Right a

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
      program {input = (VarDeclaration "N" (PType PTInt)) : input program, stmt = Seq (G.Assign "N" (LitI n)) (stmt program)}
    parseGCLfileWithN' Nothing       (Right program)  =
      program {input = (VarDeclaration "N" (PType PTInt)) : input program}
    parseGCLfileWithN' _             (Left _)         = error $ path ++ " failed to parse"