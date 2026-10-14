module ShadowElimination where

import qualified GCLParser.GCLDatatype hiding (Stmt)
import qualified GCLParser.GCLDatatype as G
import Control.Monad.State.Lazy
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map

import qualified Types as T

-- When a string key is in the map, that means that a var binding with that name was found
-- before in the program. If the value corresponding to that string is the empty list,
-- it means that in the current scope during the parsing, there are no variables with the same name,
-- meaning no variable shadowing occurs. In other words, no renaming will be necessary
-- If the list is non-empty, the strings in the list emulate a stack of all the renamed variables
-- corresponding to the current var name
type CurrentScopedVariableRenamings = Map.Map String [String]

-- A map retaining the information of the original names of the variables.
type MapFromTempsToOriginalVarNames = Map.Map String String

type S = (Int, CurrentScopedVariableRenamings, MapFromTempsToOriginalVarNames)
type ModType a = a -> State S a

-- getters
fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b
thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

shadowElim :: G.Program -> (G.Program, S)
shadowElim prog = runState (shadowElim' prog) ((1, Map.empty, Map.empty) :: S)

mkVar :: Int -> String
mkVar n = "_q" ++ show n


-- | Found a variable which is not a declaration so just rename it
replaceVar :: ModType String
replaceVar str = do
    idsToTemps <- gets snd3
    let replStr = case Map.lookup str idsToTemps of
                        Just (x:_) -> x
                        Just [] -> str
                        Nothing -> error "Error: Variable \"" ++ str ++ "\" not in scope"
     in return replStr

-- | New identifier found (input parameter or block statement)
mkFreshVar :: String -> State S (Bool, String)
mkFreshVar str = do
    (n, idsToTemps, tempsToIds) <- get
    let alterf Nothing  = (False, str, Just [])
        alterf (Just l) = let newVar = mkVar n in (True, newVar, Just (newVar:l))
        (bool, newStr, newMapIToT) = Map.alterF alterf str idsToTemps
     in (bool, newStr) <$ if bool then put (n+1, newMapIToT, Map.insert newStr str tempsToIds)
                                  else put (n  , newMapIToT, tempsToIds)
        

popVar :: String -> State S ()
popVar s = modify  $ \(n,idsToTemps, tempsToIds) -> (n, Map.adjust tail s idsToTemps, tempsToIds)

popVars :: [(Bool, G.VarDeclaration)] -> State S ()
popVars l = mapM_ popVar $ map (\(_,G.VarDeclaration s _) -> s) $ filter fst l

renameDecl :: G.VarDeclaration -> State S (Bool, G.VarDeclaration)
renameDecl (G.VarDeclaration s t) = do
       (bool, newStr) <- mkFreshVar s
       return (bool, G.VarDeclaration newStr t)

renameDecls ::  [G.VarDeclaration] -> State S [(Bool, G.VarDeclaration)]
renameDecls l = mapM renameDecl l


renameExpr :: ModType G.Expr
renameExpr (G.Var s) = G.Var <$> replaceVar s

renameExpr (G.LitI n) = return $ G.LitI n
renameExpr (G.LitB b) = return $ G.LitB b
renameExpr (G.LitNull) = return G.LitNull
renameExpr (G.Parens expr) = G.Parens <$> renameExpr expr
renameExpr (G.ArrayElem expr1 expr2) = do newExpr1 <- renameExpr expr1
                                          newExpr2 <- renameExpr expr2
                                          return $ G.ArrayElem newExpr1 newExpr2
renameExpr (G.OpNeg expr) = G.OpNeg <$> renameExpr expr
renameExpr (G.BinopExpr op expr1 expr2) = do newExpr1 <- renameExpr expr1
                                             newExpr2 <- renameExpr expr2
                                             return $ G.BinopExpr op newExpr1 newExpr2
renameExpr (G.Forall s expr) = do (bool, newStr) <- mkFreshVar s
                                  newExpr <- renameExpr expr
                                  if bool
                                    then popVar s
                                    else return ()
                                  return $ G.Forall newStr newExpr
renameExpr (G.Exists s expr) = do (bool, newStr) <- mkFreshVar s
                                  newExpr <- renameExpr expr
                                  if bool
                                    then popVar s
                                    else return ()
                                  return $ G.Exists newStr newExpr
renameExpr (G.SizeOf expr) = G.SizeOf <$> renameExpr expr
renameExpr (G.RepBy expr1 expr2 expr3) = do new1 <- renameExpr expr1
                                            new2 <- renameExpr expr2
                                            new3 <- renameExpr expr3
                                            return $ G.RepBy new1 new2 new3
renameExpr (G.Cond expr1 expr2 expr3)  = do new1 <- renameExpr expr1
                                            new2 <- renameExpr expr2
                                            new3 <- renameExpr expr3
                                            return $ G.Cond new1 new2 new3
renameExpr (G.NewStore expr)           = G.NewStore <$> renameExpr expr
renameExpr (G.Dereference str)      = G.Dereference <$> replaceVar str

renameStmt  :: ModType G.Stmt
renameStmt (G.Skip) = return G.Skip
renameStmt (G.Assert expr) = G.Assert <$> renameExpr expr
renameStmt (G.Assume expr) = G.Assume <$> renameExpr expr
renameStmt (G.Assign str expr) = do reStr <- replaceVar str
                                    newExpr <- renameExpr expr
                                    return $ G.Assign reStr newExpr
renameStmt (G.AAssign str expr1 expr2) = do reStr <- replaceVar str
                                            newExpr1 <- renameExpr expr1
                                            newExpr2 <- renameExpr expr2
                                            return $ G.AAssign reStr newExpr1 newExpr2
renameStmt (G.DrefAssign str expr) = do reStr <- replaceVar str
                                        newExpr <- renameExpr expr
                                        return $ G.Assign reStr newExpr
renameStmt (G.Seq stmt1 stmt2) = do newStmt1 <- renameStmt stmt1
                                    newStmt2 <- renameStmt stmt2
                                    return $ G.Seq newStmt1 newStmt2
renameStmt (G.IfThenElse expr stmt1 stmt2) = do newExpr <- renameExpr expr
                                                newStmt1 <- renameStmt stmt1
                                                newStmt2 <- renameStmt stmt2
                                                return $ G.IfThenElse newExpr newStmt1 newStmt2
renameStmt (G.While expr stmt) = do newExpr <- renameExpr expr
                                    newStmt <- renameStmt stmt
                                    return $ G.While newExpr newStmt
renameStmt (G.Block vars stmt) = do l <- renameDecls vars
                                    newStmt <- renameStmt stmt
                                    popVars l
                                    return $ G.Block (map snd l) newStmt
renameStmt (G.TryCatch str stmt1 stmt2) = do (bool, newStr) <- mkFreshVar str
                                             newStmt1 <- renameStmt stmt1
                                             newStmt2 <- renameStmt stmt2
                                             if bool
                                             then popVar str
                                             else return ()
                                             return $ G.TryCatch newStr newStmt1 newStmt2
                                           

shadowElim' :: ModType G.Program
shadowElim' (G.Program name input output stmt) = do
   (bools, inputs) <- unzip <$> renameDecls input
   stmt'   <- renameStmt stmt
   if (or bools) then error "Input declaration duplication" else return ()
   (bools', outputs) <- unzip <$> renameDecls output
   if (not $ and bools') then error "Output variable is not defined" else return ()
   return $ G.Program name  inputs  outputs stmt'


   
