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


type ExprAlgebra r = (
    String -> r,             -- var
    Int -> r,                -- liti
    Bool -> r,               -- litb
    r,                       -- litnull
    r -> r -> r,
    r -> r,
    G.BinOp -> r -> r -> r,
    String -> r -> r,
    String -> r -> r,
    r -> r,
    r -> r -> r -> r,
    r -> r -> r -> r,
    r -> r,
    String -> r
 )
