module Main (main) where

import GCLParser.Parser
import GCLParser.GCLDatatype
import GCLParser.PrettyPrint

test :: IO (Program)
test = do
 Right(prog) <- parseGCLfile "./gclparser/examples/E.gcl"
 return prog

main :: IO ()
main = test >>= (putStrLn . ppProgram2String)
