module Main (main) where

import GCLParser.Parser

import qualified GCLParser.GCLDatatype as G
import qualified  GCLParser.PrettyPrint as G.PP

import Data.Tree
import Data.Maybe (mapMaybe)
import Text.PrettyPrint (Doc, text, char, nest, vcat, ($$))
import GCLParser.GCLDatatype (Expr(BinopExpr))
import Z3.Base as Z

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

wlp :: PathTree -> [G.Expr]
wlp (Node x []) = [wlp' x (G.LitB True)]
wlp (Node x ts) = map (wlp' x) (concat $ map wlp ts)

wlp' :: Stmt -> G.Expr -> G.Expr
wlp'  Skip               q  = q
wlp' (Assert    e)       q  = G.opAnd e q
wlp' (Assume    e)       q  = G.opImplication e q
wlp' (Assign  s e)       q  = (foldExpr exprReplaceAlgebra q) s e
wlp' (AAssign s e e2)    q  = (foldExpr exprRepbyAlgebra   q) s e e2

exprIdAlgebra :: ExprAlgebra Expr
exprIdAlgebra = (
    \s        -> G.Var s,
    \i        -> G.LitI i,
    \b        -> G.LitB b,
                 G.LitNull,
    \e e2     -> G.ArrayElem e e2,
    \e        -> G.OpNeg e,
    \bop e e2 -> G.BinopExpr bop e e2,
    \s e      -> G.Forall s e,
    \s e      -> G.Exists s e,
    \e        -> G.SizeOf e,
    \e e2 e3  -> G.RepBy e e2 e3,
    \e e2 e3  -> G.Cond e e2 e3,
    \e        -> G.NewStore e,
    \s        -> G.Dereference s )
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

foldExpr :: ExprAlgebra r -> Expr -> r
foldExpr (var, liti, litb, litnull, arrayelem, opneg, binopexpr, foral, exists, sizeof, repby, cond, newstore, dereference) = f
    where
        f (G.Var s)              = var s
        f (G.LitI n)             = liti n
        f (G.LitB b)             = litb b
        f (G.LitNull)            = litnull
        f (G.Parens e)           = f e -- discard parens
        f (G.ArrayElem e e2)     = arrayelem (f e) (f e2)
        f (G.OpNeg e)            = opneg (f e)
        f (G.BinopExpr bop e e2) = binopexpr bop (f e) (f e2)
        f (G.Forall s e)         = foral s (f e)
        f (G.Exists s e)         = exists s (f e)
        f (G.SizeOf e)           = sizeof (f e)
        f (G.RepBy e e2 e3)      = repby (f e) (f e2) (f e3)
        f (G.Cond e e2 e3)       = cond (f e) (f e2) (f e3)
        f (G.NewStore e)         = newstore (f e)
        f (G.Dereference s)      = dereference s

exprReplaceAlgebra :: ExprAlgebra (String -> Expr -> Expr)
exprReplaceAlgebra = (
    \s        rps rpe -> (if rps == s then rpe else G.Var s),
    \i        _   _   -> G.LitI i,
    \b        _   _   -> G.LitB b,
    \         _   _   -> G.LitNull,

    \e e2     rps rpe -> G.ArrayElem (e rps rpe) (e2 rps rpe),
    \e        rps rpe -> G.OpNeg (e rps rpe),
    \bop e e2 rps rpe -> G.BinopExpr bop (e rps rpe) (e2 rps rpe),
    \s e      rps rpe -> G.Forall s (e rps rpe),
    \s e      rps rpe -> G.Exists s (e rps rpe),
    \e        rps rpe -> G.SizeOf (e rps rpe),
    \e e2 e3  rps rpe -> G.RepBy (e rps rpe) (e2 rps rpe) (e3 rps rpe),
    \e e2 e3  rps rpe -> G.Cond (e rps rpe) (e2 rps rpe) (e3 rps rpe),
    \e        rps rpe -> G.NewStore (e rps rpe),
    \s        _   _   -> G.Dereference s )

exprRepbyAlgebra :: ExprAlgebra (String -> Expr -> Expr -> Expr)
exprRepbyAlgebra = (
    \s         _   _   _   -> G.Var s,
    \i         _   _   _   -> G.LitI i,
    \b         _   _   _   -> G.LitB b,
    \          _   _   _   -> G.LitNull,
    \var index ars rpi rpe -> if (getVarName (var ars rpi rpe) == ars)
        then (G.Cond (G.BinopExpr G.Equal (index ars rpi rpe) (rpi)) (rpe) (G.ArrayElem (var ars rpi rpe) (index ars rpi rpe)))
        else (G.ArrayElem (var ars rpi rpe) (index ars rpi rpe)),
    \e         ars rpi rpe -> G.OpNeg (e ars rpi rpe),
    \bop e e2  ars rpi rpe -> G.BinopExpr bop (e ars rpi rpe) (e2 ars rpi rpe),
    \s e       ars rpi rpe -> G.Forall s (e ars rpi rpe),
    \s e       ars rpi rpe -> G.Exists s (e ars rpi rpe),
    \e         ars rpi rpe -> G.SizeOf (e ars rpi rpe),
    \e e2 e3   ars rpi rpe -> G.RepBy (e ars rpi rpe) (e2 ars rpi rpe) (e3 ars rpi rpe),
    \e e2 e3   ars rpi rpe -> G.Cond (e ars rpi rpe) (e2 ars rpi rpe) (e3 ars rpi rpe),
    \e         ars rpi rpe -> G.NewStore (e ars rpi rpe),
    \s         _   _   _   -> G.Dereference s
    )
    where getVarName (G.Var s) = s
          getVarName _ = error "unreachable"
exprVerbEqAlgebra :: ExprAlgebra Expr
exprVerbEqAlgebra = let
    verbatimEquality bop e e2 | bop `elem` [G.Equal, G.GreaterThanEqual, G.LessThanEqual] && e == e2 = G.LitB True
                              | bop == G.Equal                                            && e /= e2 = G.LitB False
                              | bop `elem` [G.LessThan, G.GreaterThan]                    && e == e2 = G.LitB False
                              | otherwise = G.BinopExpr bop e e2
 in (
    \s        -> G.Var s,
    \i        -> G.LitI i,
    \b        -> G.LitB b,
    G.LitNull,
    \e e2     -> G.ArrayElem e e2,
    \e        -> G.OpNeg e,
    verbatimEquality,
    \s e      -> G.Forall s e,
    \s e      -> G.Exists s e,
    \e        -> G.SizeOf e,
    \e e2 e3  -> G.RepBy e e2 e3,
    \e e2 e3  -> G.Cond e e2 e3,
    \e        -> G.NewStore e,
    \s        -> G.Dereference s )

exprTautologyAlgebra :: ExprAlgebra Expr
exprTautologyAlgebra = let
    tautology G.And e e2  | e == G.LitB (False) || e2 == G.LitB (False) = G.LitB (False)
                          | e == G.LitB (True) = e2
                          | e2 == G.LitB (True) = e
                          | otherwise = G.opAnd e e2
    tautology G.Or  e e2  | e == G.LitB (True) || e2 == G.LitB (True) = G.LitB (True)
                          | e == G.LitB (False) = e2
                          | e2 == G.LitB (False) = e
                          | otherwise = G.opOr     e e2
    tautology G.Implication e e2 | e == G.LitB (False) = G.LitB (True)
                                 | e2 == G.LitB (True) = G.LitB (True)
                                 | e  == G.LitB (True) && e2 == G.LitB (False) = G.LitB (False)
                                 | e2 == G.LitB (False)                        = G.OpNeg e
                                 | otherwise = G.opImplication e e2
    tautology bop e e2 = G.BinopExpr bop e e2
    cond (G.LitB True)  e _  = e
    cond (G.LitB False) _ e2 = e2
    cond g              e e2 = G.Cond g e e2
    opneg (G.LitB True)      = G.LitB False
    opneg (G.LitB False)     = G.LitB True
    opneg e                  = e
  in (
    \s        -> G.Var s,
    \i        -> G.LitI i,
    \b        -> G.LitB b,
    G.LitNull,
    \e e2     -> G.ArrayElem e e2,
    opneg,
    tautology,
    \s e      -> G.Forall s e,
    \s e      -> G.Exists s e,
    \e        -> G.SizeOf e,
    \e e2 e3  -> G.RepBy e e2 e3,
    \e e2 e3  -> cond e e2 e3,
    \e        -> G.NewStore e,
    \s        -> G.Dereference s )

exprIntCalcAlgebra :: ExprAlgebra Expr
exprIntCalcAlgebra = (
    \s        -> G.Var s,
    \i        -> G.LitI i,
    \b        -> G.LitB b,
                 G.LitNull,
    \e e2     -> G.ArrayElem e e2,
    \e        -> G.OpNeg e,
    \bop e e2 -> calc bop e e2,
    \s e      -> G.Forall s e,
    \s e      -> G.Exists s e,
    \e        -> G.SizeOf e,
    \e e2 e3  -> G.RepBy e e2 e3,
    \e e2 e3  -> G.Cond e e2 e3,
    \e        -> G.NewStore e,
    \s        -> G.Dereference s )
    where
      calc G.Plus     (G.LitI a) (G.LitI b)                          = G.LitI (a + b)
      calc G.Plus     ((G.BinopExpr G.Plus  e (G.LitI a))) (G.LitI b) = G.BinopExpr G.Plus e (G.LitI (a + b))
      calc G.Plus     ((G.BinopExpr G.Minus e (G.LitI a))) (G.LitI b) | a == b    = e
                                                                      | a > b     = G.BinopExpr G.Minus e (G.LitI (a - b))
                                                                      | otherwise = G.BinopExpr G.Plus  e (G.LitI (b - a))
      calc G.Minus    (G.LitI a) (G.LitI b) = G.LitI (a - b)
      calc G.Minus    ((G.BinopExpr G.Minus e (G.LitI a))) (G.LitI b) = G.BinopExpr G.Minus e (G.LitI (a + b))
      calc G.Minus    ((G.BinopExpr G.Plus  e (G.LitI a))) (G.LitI b) | a == b    = e
                                                                      | a < b     = G.BinopExpr G.Minus e (G.LitI (b - a))
                                                                      | otherwise = G.BinopExpr G.Plus  e (G.LitI (a - b))
      calc G.Multiply (G.LitI a) (G.LitI b)                              = G.LitI (a * b)
      calc G.Multiply ((G.BinopExpr G.Multiply e (G.LitI a))) (G.LitI b) = G.BinopExpr G.Multiply e (G.LitI (a * b))
      calc G.Multiply ((G.BinopExpr G.Multiply (G.LitI a) e)) (G.LitI b) = G.BinopExpr G.Multiply e (G.LitI (a * b))
      calc G.Divide   (G.LitI a) (G.LitI b)                              = G.LitI (div a b)
      calc G.Divide   ((G.BinopExpr G.Divide   e (G.LitI a))) (G.LitI b) = G.BinopExpr G.Divide   e (G.LitI (a * b))
      calc G.Divide   ((G.BinopExpr G.Divide   (G.LitI a) e)) (G.LitI b) = G.BinopExpr G.Divide   e (G.LitI (a * b))
      calc G.LessThan         (G.LitI a) (G.LitI b) = G.LitB (a < b)
      calc G.LessThanEqual    (G.LitI a) (G.LitI b) = G.LitB (a <= b)
      calc G.GreaterThan      (G.LitI a) (G.LitI b) = G.LitB (a > b)
      calc G.GreaterThanEqual (G.LitI a) (G.LitI b) = G.LitB (a >= b)
      calc bop e e2 = G.BinopExpr bop e e2

exprRewriteAlgebra :: ExprAlgebra Expr
exprRewriteAlgebra = (
    \s        -> G.Var s,
    \i        -> G.LitI i,
    \b        -> G.LitB b,
                 G.LitNull,
    \e e2     -> G.ArrayElem e e2,
    \e        -> G.OpNeg e,
    rewriteImplication,
    \s e      -> G.Forall s e,
    \s e      -> G.Exists s e,
    \e        -> G.SizeOf e,
    \e e2 e3  -> G.RepBy e e2 e3,
    \e e2 e3  -> G.Cond e e2 e3,
    \e        -> G.NewStore e,
    \s        -> G.Dereference s )
    where 
      rewriteImplication G.Implication e e2 = G.opOr (G.OpNeg e) (e2)
      rewriteImplication bop           e e2 = G.BinopExpr bop e e2

-- TODO: dealing with minus in LitI (is this necessary?)

--------------------------------------------------------------------------------

test :: IO [Expr]
test = do
    Right(prog) <- parseGCLfile "./gclparser/examples/test.gcl"
    let paths = mapMaybe (truncatePaths 20) $ unroll $ G.stmt prog
    return $ concatMap wlp paths

optimizeUntilIdempotence exprs = let
    optimized = foldr1 (.) (map foldExpr [
        exprVerbEqAlgebra,
        exprTautologyAlgebra,
        exprIntCalcAlgebra,
        exprRewriteAlgebra
     ]) exprs;
 in if optimized == exprs then exprs else optimizeUntilIdempotence optimized

main :: IO ()
main = let
 in do
    exprs <- test
    let optimized = map optimizeUntilIdempotence exprs
    _ <- mapM (putStrLn . show . G.PP.ppExpr) optimized
    return ()
