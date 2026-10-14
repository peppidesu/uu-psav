{-# LANGUAGE LambdaCase, GADTs #-}

module SMT (
Assignments,
z3Check,
getTypes,
Types(..),
VarTypes
)
where

import qualified GCLParser.GCLDatatype as G
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import Control.Monad.State
import Control.Monad (when)
import Z3.Monad

-- Basic types are either Int or Bool
data PrimType a where
   PInt :: PrimType Int
   PBool :: PrimType Bool

instance Eq (PrimType a) where
   _ == _ = True

instance Show (PrimType a) where
  show PInt = "PInt"
  show PBool = "PBool"

-- Arrays are just 1-dimensional array of a primitive type
data AType a where
   AType :: PrimType a -> AType a

instance Eq (AType a) where
   _ == _ = True

instance Show (AType a) where
   show (AType t) = "AType " ++ show t

-- a variable type in this project is either a primitive type
-- or an array of a primitive type
data Type a where
   PrimT :: PrimType a -> Type        a
   ArrT  :: AType    a -> Type (AType a)

instance Eq (Type a) where
   (PrimT _) == (PrimT _) = True
   (ArrT _)  == (ArrT  _) = True
   _         == _         = False

instance Show (Type a) where
   show (PrimT t) = "PrimT " ++ show t
   show (ArrT  t) = "ArrT  " ++ show t


-- An ADT instance of the possible types
data Types = TInt | TBool | TAInt | TABool | TRef
  deriving (Eq, Show, Ord)


-- a simple VarTypes map that maps a variable to its type
-- necessary since the expression does not carry the type of the variables anywhere
-- currently
type VarTypes = Map.Map String Types



getTypesExpr :: G.Expr -> State VarTypes ()
getTypesExpr (G.Parens expr) = getTypesExpr expr
getTypesExpr (G.OpNeg  expr) = getTypesExpr expr
getTypesExpr (G.BinopExpr _ e1 e2) = getTypesExpr e1 >> getTypesExpr e2
getTypesExpr (G.Forall str expr)   = getTypesVarDecl (G.VarDeclaration str (G.PType G.PTInt)) >> getTypesExpr expr
getTypesExpr (G.Exists str expr)   = getTypesVarDecl (G.VarDeclaration str (G.PType G.PTInt)) >> getTypesExpr expr
getTypesExpr (G.RepBy e1 e2 e3) = mapM_ getTypesExpr [e1,e2,e3]
getTypesExpr (G.Cond  e1 e2 e3) = mapM_ getTypesExpr [e1,e2,e3]
getTypesExpr (G.ArrayElem e1 e2) = mapM_ getTypesExpr [e1,e2]
getTypesExpr _  = return ()

-- assume that the program has done shadow elimination
getTypes :: G.Program -> VarTypes
getTypes (G.Program _name inputs outputs stmt) =
     flip execState Map.empty $ do getTypesVarDecls (outputs ++ inputs)
                                   getTypesStmt stmt


getTypesVarDecl :: G.VarDeclaration -> State VarTypes ()
getTypesVarDecl (G.VarDeclaration str t) = modify $ Map.insert str t'
    where t' = case t of
                 G.PType G.PTInt -> TInt
                 G.PType G.PTBool -> TBool
                 G.AType G.PTInt -> TAInt
                 G.AType G.PTBool -> TABool
                 G.RefType       -> TRef

getTypesVarDecls :: [G.VarDeclaration] -> State VarTypes ()
getTypesVarDecls = mapM_ getTypesVarDecl

getTypesStmt :: G.Stmt -> State VarTypes ()
getTypesStmt = go

  where go = \case
                G.Seq stmt1 stmt2          -> go stmt1 >> go stmt2
                G.IfThenElse e stmt1 stmt2 -> getTypesExpr e >> go stmt1 >> go stmt2
                G.While e stmt             -> getTypesExpr e >> go stmt
                G.Block l stmt             -> getTypesVarDecls l >>  go stmt
                G.TryCatch _ _ _           -> error "Error SMT.hs: We do not handle tryCatch yet"
                G.Assert e                 -> getTypesExpr e
                G.Assume e                 -> getTypesExpr e
                G.Assign _ e               -> getTypesExpr e
                G.AAssign _ e1 e2          -> getTypesExpr e1 >> getTypesExpr e2
                G.DrefAssign _ e           -> getTypesExpr e
                _                          -> return ()


myMkVar :: MonadZ3 z3 => String -> Types -> StateT VarsToASTs z3 AST
myMkVar str types = do   map <- get
                         case (map !? str) of
                           Nothing -> do astVar <- lift $ myMkVar' str types
                                         (put $ Map.insert str astVar map)
                                         return astVar
                           Just astVar -> return astVar


myMkVar' :: MonadZ3 z3 => String -> Types -> z3 AST
myMkVar' str t = do symbol <- mkStringSymbol str
                    case t of
                     TInt  -> mkIntVar  symbol
                     TBool -> mkBoolVar symbol
                     TAInt -> do intSort <- mkIntSort
                                 intArrSort <- mkArraySort intSort intSort
                                 mkVar symbol intArrSort
                     TABool -> do intSort <- mkIntSort
                                  boolSort <- mkBoolSort
                                  boolArrSort <- mkArraySort boolSort intSort
                                  mkVar symbol boolArrSort
                     TRef  -> error "z3: not implemented yet"

myMkArrSize :: MonadZ3 z3 => String -> StateT VarsToASTs z3 AST
myMkArrSize str = do map <- get
                     case (map !? str) of
                       Nothing -> do arrSizeVar <-  myMkVar str TInt
                                     zero <- lift $ mkIntSort >>= mkInt 0
                                     lift (mkLe zero arrSizeVar >>= assert)
                                     return arrSizeVar
                       Just ast  ->  return ast


myMkBinOp :: MonadZ3 z3 => G.BinOp -> AST -> AST -> StateT VarsToASTs z3 AST
myMkBinOp binOp a b = lift $ cast binOp
  where cast G.And = mkAnd [a,b]
        cast G.Or  = mkOr  [a,b]
        cast G.Implication = mkImplies a b
        cast G.LessThan    = mkLt a b
        cast G.LessThanEqual = mkLe a b
        cast G.GreaterThan   = mkGt a b
        cast G.GreaterThanEqual = mkGe a b
        cast G.Equal            = mkEq a b
        cast G.Minus            = mkSub [a,b]
        cast G.Plus             = mkAdd [a,b]
        cast G.Multiply         = mkMul [a,b]
        cast G.Divide           = mkRem a b
        cast G.Alias            = error "z3: No support for Alias"

-- vars to asts
type VarsToASTs = Map.Map String AST

-- Assignments stringified in case a counterexample is possible
type Assignments = [(String, String)]
getAssignments :: MonadZ3 z3 => Model -> VarsToASTs -> z3 Assignments
getAssignments model map =
  mapM printOne $ Map.toList map
  where
    printOne (name, ast) = do
      value <- modelEval model ast True
      case value of
        Just v -> do
           str <- astToString v
           return $ (name,str)
        Nothing ->
           return $ (name,"<unknown>")


z3Check :: VarTypes -> G.Expr -> IO (Maybe Assignments)
z3Check types expr = evalZ3 $ z3Check' types expr

-- if it returns Just, those are counterexamples. Otherwise, the expr is satisfiable
z3Check' :: MonadZ3 z3 => VarTypes -> G.Expr -> z3 (Maybe Assignments)
z3Check' types expr = do (ast, map) <- runStateT (smtAST types expr) Map.empty
                         ast >>= assert
                         (_, maybeModel) <- getModel
                         case maybeModel of
                          Nothing -> return Nothing
                          Just model -> Just <$> getAssignments model map


-- Create the z3 AST analogue of the Expression given to check for satisfyibility.
smtAST :: MonadZ3 z3 => VarTypes -> G.Expr -> StateT VarsToASTs z3 AST
smtAST map =
    \case
      G.Var str -> case map !? str of
                     Nothing -> error $ "z3: Couldn't find type for " ++ str
                     Just t -> myMkVar str t

      G.LitI n -> lift (mkIntSort >>=  mkInt n)
      G.LitB b -> lift $ mkBool b
      G.LitNull -> error $ "z3: LitNull literal has not been implemented"
      G.Parens expr -> smtAST map expr
      G.ArrayElem  arrExpr ixExpr -> do arr <- smtAST map arrExpr
                                        ix  <- smtAST map ixExpr
                                        lift $ mkSelect arr ix
      G.OpNeg expr -> smtAST map expr >>= lift . mkNot

      G.BinopExpr binOp expr1 expr2 -> do ast1 <- smtAST map expr1
                                          ast2 <- smtAST map expr2
                                          myMkBinOp binOp ast1 ast2

      G.Forall str expr -> do var  <- myMkVar str TInt
                              body <- smtAST map expr
                              var' <- lift $ toApp var
                              lift $ mkForallConst [] [var'] body

      G.Exists str expr -> do var  <- myMkVar str TInt
                              body <- smtAST map expr
                              var' <- lift $ toApp var
                              lift $ mkExistsConst [] [var'] body

      G.SizeOf expr | G.Var a <- expr ->  myMkArrSize $ a ++ "_size"
                    | otherwise       -> error "z3: haven't implemented complicated sizeOf"

      G.RepBy  e1 e2 e3     -> do expr1 <- smtAST map e1
                                  expr2 <- smtAST map e2
                                  expr3 <- smtAST map e3
                                  lift $ mkStore expr1 expr2 expr3

      G.Cond  cond e1 e2 -> do condExpr <- smtAST map cond
                               expr1    <- smtAST map e1
                               expr2    <- smtAST map e2
                               lift $ mkIte condExpr expr1 expr2

      G.NewStore expr    -> undefined

      G.Dereference expr -> undefined
