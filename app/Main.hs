module Main (main) where

import GCLParser.Parser
import qualified  GCLParser.PrettyPrint (ppStmt, ppExpr)

import qualified GCLParser.GCLDatatype as G
import Data.Tree
import Data.Maybe (mapMaybe, catMaybes)
import Text.PrettyPrint (Doc, text, char, nest, vcat, ($$))

data Stmt
    = Skip
    | Assert      G.Expr
    | Assume      G.Expr
    | Assign      String           G.Expr
    | AAssign     String           G.Expr   G.Expr
    | Unsupported G.Stmt

instance Show Stmt where
    show Skip                   = "skip"
    show (Assert cond)          = "assert " ++ show cond
    show (Assume cond)          = "assume " ++ show cond
    show (Assign ident e)       = ident ++ " := " ++ show e
    show (AAssign ident i e)    = ident ++ "[" ++ show i ++ "]" ++ " := " ++ show e
    show (Unsupported s)        = "unsupported < " ++ show s ++ " >"

type PathTree = Tree Stmt


--- pretty print for PathTree
ppPathTree2String :: PathTree -> String
ppPathTree2String pt = show . ppPathTree $ pt

ppPathTree :: PathTree -> Doc
ppPathTree = go
  where
    go (Node s [])     = ppStmt s
    go (Node s cs)     = ppStmt s $$ nest 2 (vcat (map go cs))

ppStmt :: Stmt -> Doc
ppStmt Skip            = text "skip"
ppStmt (Assert e)      = text "assert " <> GCLParser.PrettyPrint.ppExpr e
ppStmt (Assume e)      = text "assume " <> GCLParser.PrettyPrint.ppExpr e
ppStmt (Assign x e)    = text x <> text " := " <> GCLParser.PrettyPrint.ppExpr e
ppStmt (AAssign x i e) = text x <> char '[' <> GCLParser.PrettyPrint.ppExpr i <> char ']' <> text " := " <> GCLParser.PrettyPrint.ppExpr e
ppStmt (Unsupported s) = text "Unsupported" <> GCLParser.PrettyPrint.ppStmt s

concatToLeaves :: [PathTree] -> [PathTree] -> [PathTree]
concatToLeaves t = map (foldTree $ \x ts -> Node x (if null ts then t else ts))

unrollIfThenElse :: G.Expr -> G.Stmt -> G.Stmt -> [PathTree]
unrollIfThenElse cond sIf sElse = [
    Node (Assume cond)           (unroll sIf),
    Node (Assume (G.OpNeg cond)) (unroll sElse)
 ]

unrollWhile :: G.Expr -> G.Stmt -> [PathTree]
unrollWhile cond body = [
    Node (Assume cond)           (concatToLeaves (unrollWhile cond body) (unroll body)),
    Node (Assume (G.OpNeg cond)) []
 ]

------ unfold ------

unroll :: G.Stmt -> [PathTree]
unroll (G.Seq s1 s2) = concatToLeaves (unroll s2) (unroll s1)

unroll (G.IfThenElse cond sIf sElse)
    = unrollIfThenElse cond sIf sElse
unroll (G.While cond body)
    = unrollWhile cond body

unroll (G.Skip)                 = [pure $ Skip]
unroll (G.Assume e)             = [pure $ Assume e]
unroll (G.Assert e)             = [pure $ Assert e]
unroll (G.Assign ident e)       = [pure $ Assign ident e]
unroll (G.AAssign ident eIdx e) = [pure $ AAssign ident eIdx e]
unroll stmt                     = [pure $ Unsupported stmt]

--------------------

-- unroll produces a lazy infinite tree, so it needs to be truncated first.
truncatePaths :: Integer -> PathTree -> Maybe (PathTree)
truncatePaths _ node@(Node _ []) = Just node
truncatePaths k (Node x ts)
    | k <= 0    = Nothing -- max depth reached -> prune
    | otherwise = case mapMaybe (truncatePaths (k - 1)) ts of
        []  -> Nothing -- all successors truncated, path cannot terminate -> prune
        ts' -> Just (Node x ts') -- terminating successors -> don't prune


test :: IO [PathTree]
test = do
    Right(prog) <- parseGCLfile "./gclparser/examples/E.gcl"
    return $ mapMaybe (truncatePaths 20) $ unroll $ G.stmt prog

main :: IO ()
main = do
    pts <- test
    _ <- mapM (putStrLn . ppPathTree2String) pts
    return ()
