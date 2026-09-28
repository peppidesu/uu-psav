module PrettyPrint where
import Types

import Text.PrettyPrint
import Prelude hiding ((<>))
import Data.Tree
import GCLParser.PrettyPrint (ppExpr)

-- |Pretty prints a `PathTree`
ppPathTree2String :: PathTree -> String
ppPathTree2String pt = show . ppPathTree $ pt

ppPathTree :: PathTree -> Doc
ppPathTree = go
    where
    go (Node s [])     = ppStmt s
    go (Node s cs)     = ppStmt s $$ nest 2 (vcat (map go cs))

ppStmt :: Stmt -> Doc
ppStmt Skip            = text "skip"
ppStmt (Assert e)      = text "assert " <> ppExpr e
ppStmt (Assume e)      = text "assume " <> ppExpr e
ppStmt (Assign x e)    = text x <> text " := " <> ppExpr e
ppStmt (AAssign x i e) = text x <> char '[' <> ppExpr i <> char ']' <> text " := " <> ppExpr e
