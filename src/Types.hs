{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFoldable #-}
module Types where

import Data.Tree (Tree (..))
import qualified GCLParser.GCLDatatype as G


data Stmt
    = Skip
    | Assert      G.Expr
    | Assume      G.Expr
    | Assign      String           G.Expr
    | AAssign     String           G.Expr   G.Expr

instance Show Stmt where
    show Skip                   = "skip"
    show (Assert cond)          = "assert " ++ show cond
    show (Assume cond)          = "assume " ++ show cond
    show (Assign ident e)       = ident ++ " := " ++ show e
    show (AAssign ident i e)    = ident ++ "[" ++ show i ++ "]" ++ " := " ++ show e


-- |Tree of program paths. Each node is a statement, and its children
-- are next statements that can be executed. Exploring to a leaf node produces
-- a program path.
type PathTree = Tree Stmt

-- https://wiki.haskell.org/Catamorphisms
data ExprF a
    = VarF         String
    | LitIF        Int
    | LitBF        Bool
    | LitNullF
    | ArrayElemF   a a
    | OpNegF       a
    | BinopExprF   G.BinOp a a
    | ForallF      String a
    | ExistsF      String a
    | SizeOfF      a
    | RepByF       a a a
    | CondF        a a a
    | NewStoreF    a
    | DereferenceF String
  deriving (Eq, Show, Functor)


project :: G.Expr -> ExprF G.Expr
project = \case
    G.Var s             -> VarF s
    G.LitI n            -> LitIF n
    G.LitB b            -> LitBF b
    G.LitNull           -> LitNullF
    G.Parens e          -> project e            -- discard parens
    G.ArrayElem e1 e2   -> ArrayElemF e1 e2
    G.OpNeg e           -> OpNegF e
    G.BinopExpr b e1 e2 -> BinopExprF b e1 e2
    G.Forall s e        -> ForallF s e
    G.Exists s e        -> ExistsF s e
    G.SizeOf e          -> SizeOfF e
    G.RepBy e1 e2 e3    -> RepByF e1 e2 e3
    G.Cond e1 e2 e3     -> CondF e1 e2 e3
    G.NewStore e        -> NewStoreF e
    G.Dereference s     -> DereferenceF s

embed :: ExprF G.Expr -> G.Expr
embed = \case
    VarF s             -> G.Var s
    LitIF n            -> G.LitI n
    LitBF b            -> G.LitB b
    LitNullF           -> G.LitNull
    ArrayElemF e1 e2   -> G.ArrayElem e1 e2
    OpNegF e           -> G.OpNeg e
    BinopExprF b e1 e2 -> G.BinopExpr b e1 e2
    ForallF s e        -> G.Forall s e
    ExistsF s e        -> G.Exists s e
    SizeOfF e          -> G.SizeOf e
    RepByF e1 e2 e3    -> G.RepBy e1 e2 e3
    CondF e1 e2 e3     -> G.Cond e1 e2 e3
    NewStoreF e        -> G.NewStore e
    DereferenceF s     -> G.Dereference s


type ExprAlgebra a = ExprF a -> a
cata :: ExprAlgebra a -> G.Expr -> a
cata f = f . fmap (cata f) . project

foldExpr :: ExprAlgebra a -> G.Expr -> a
foldExpr = cata
