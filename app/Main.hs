module Main (main) where

import GCLParser.Parser

import qualified GCLParser.GCLDatatype as G
import qualified  GCLParser.PrettyPrint as G.PP

import Data.Tree
import Data.Maybe (mapMaybe)
import Text.PrettyPrint (Doc, text, char, nest, vcat, ($$))

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
ppStmt (Assert e)      = text "assert " <> G.PP.ppExpr e
ppStmt (Assume e)      = text "assume " <> G.PP.ppExpr e
ppStmt (Assign x e)    = text x <> text " := " <> G.PP.ppExpr e
ppStmt (AAssign x i e) = text x <> char '[' <> G.PP.ppExpr i <> char ']' <> text " := " <> G.PP.ppExpr e


-- | `graftToLeaves f1 f2` walks every tree in `f2` and, at every leaf
-- (a node whose child list is empty), installs the list `f1` as that node's
-- children. Interior nodes are left untouched.
--
-- In the context of `PathTree`, this is equivalent to constructing a sequence
-- of two program paths. Note that the succeeding path should be passed
-- before the preceding path in this case.
graftToLeaves :: [Tree a] -> [Tree a] -> [Tree a]
graftToLeaves t = map (foldTree $ \x ts -> Node x (if null ts then t else ts))

------------------------------------ unroll ------------------------------------

-- |Unrolls a GCL statement. This produces a forest rather than a single tree
-- because branching statements unroll into multiple paths immediately.
unroll :: G.Stmt -> [PathTree]
unroll (G.Seq s1 s2) = graftToLeaves (unroll s2) (unroll s1)

unroll (G.IfThenElse cond sIf sElse) = [
    -- unroll into each branch, prepend with `assume` of condition that should hold.
    Node (Assume cond)           (unroll sIf),
    Node (Assume (G.OpNeg cond)) (unroll sElse)
 ]

unroll while@(G.While cond body) = [
    -- unroll loop entry and loop exit scenarios.
    -- loop entry will recurse back to this statement.
    Node (Assume cond)           (graftToLeaves (unroll while) (unroll body)),
    Node (Assume (G.OpNeg cond)) []
 ]

-- primitive statements, these do not need to be unrolled.
unroll (G.Assume e)             = [pure $ Assume e]
unroll (G.Assert e)             = [pure $ Assert e]
unroll (G.Assign ident e)       = [pure $ Assign ident e]
unroll (G.AAssign ident eIdx e) = [pure $ AAssign ident eIdx e]

-- handles `skip` primitive and short-circuits statements we don't support
unroll _                        = [pure $ Skip]

-- unroll produces a lazy infinite tree, so it needs to be truncated first.
truncatePaths :: Integer -> PathTree -> Maybe (PathTree)
truncatePaths _ node@(Node _ []) = Just node
truncatePaths k (Node x ts)
    | k <= 0    = Nothing -- max depth reached -> prune
    | otherwise = case mapMaybe (truncatePaths (k - 1)) ts of
        []  -> Nothing -- all successors truncated, path cannot terminate -> prune
        ts' -> Just (Node x ts') -- terminating successors -> don't prune

--------------------------------------------------------------------------------

test :: IO [PathTree]
test = do
    Right(prog) <- parseGCLfile "./gclparser/examples/E.gcl"
    return $ mapMaybe (truncatePaths 20) $ unroll $ G.stmt prog

main :: IO ()
main = do
    pts <- test
    _ <- mapM (putStrLn . ppPathTree2String) pts
    return ()
