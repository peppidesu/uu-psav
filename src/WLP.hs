module WLP where
import Types
import GCLParser.GCLDatatype (Expr(..), BinOp (..), opAnd, opOr, opImplication)
import Data.Tree (Tree(..))

wlp :: PathTree -> [Expr]
wlp (Node x []) = [wlp' x (LitB True)]
wlp (Node x ts) = map (wlp' x) (concat $ map wlp ts)

wlp' :: Stmt -> Expr -> Expr
wlp'  Skip               q = q
wlp' (Assert    e)       q = opAnd e q
wlp' (Assume    e)       q = opImplication e q
wlp' (Assign  s e)       q = (foldExpr exprReplaceAlgebra q) s e
wlp' (AAssign s e e2)    q = (foldExpr exprRepbyAlgebra   q) s e e2

foldExpr :: ExprAlgebra r -> Expr -> r
foldExpr (var, liti, litb, litnull, arrayelem, opneg, binopexpr, foral, exists, sizeof, repby, cond, newstore, dereference) = f
  where
    f (Var s)              = var s
    f (LitI n)             = liti n
    f (LitB b)             = litb b
    f (LitNull)            = litnull
    f (Parens e)           = f e -- discard parens
    f (ArrayElem e e2)     = arrayelem (f e) (f e2)
    f (OpNeg e)            = opneg (f e)
    f (BinopExpr bop e e2) = binopexpr bop (f e) (f e2)
    f (Forall s e)         = foral s (f e)
    f (Exists s e)         = exists s (f e)
    f (SizeOf e)           = sizeof (f e)
    f (RepBy e e2 e3)      = repby (f e) (f e2) (f e3)
    f (Cond e e2 e3)       = cond (f e) (f e2) (f e3)
    f (NewStore e)         = newstore (f e)
    f (Dereference s)      = dereference s

exprReplaceAlgebra :: ExprAlgebra (String -> Expr -> Expr)
exprReplaceAlgebra = (
    \s        rps rpe -> (if rps == s then rpe else Var s),
    \i        _   _   -> LitI i,
    \b        _   _   -> LitB b,
    \         _   _   -> LitNull,

    \e e2     rps rpe -> ArrayElem     (e rps rpe) (e2 rps rpe),
    \e        rps rpe -> OpNeg         (e rps rpe),
    \bop e e2 rps rpe -> BinopExpr bop (e rps rpe) (e2 rps rpe),
    \s e      rps rpe -> Forall s      (e rps rpe),
    \s e      rps rpe -> Exists s      (e rps rpe),
    \e        rps rpe -> SizeOf        (e rps rpe),
    \e e2 e3  rps rpe -> RepBy         (e rps rpe) (e2 rps rpe) (e3 rps rpe),
    \e e2 e3  rps rpe -> Cond          (e rps rpe) (e2 rps rpe) (e3 rps rpe),
    \e        rps rpe -> NewStore      (e rps rpe),
    \s        _   _   -> Dereference s )

exprVarDeclAlgebra = (
    \s        rps rpe -> (if rps == s then rpe else Var s),
    \i        _   _   -> LitI i,
    \b        _   _   -> LitB b,
    \         _   _   -> LitNull,

    \e e2     rps rpe -> ArrayElem     (e rps rpe) (e2 rps rpe),
    \e        rps rpe -> OpNeg         (e rps rpe),
    \bop e e2 rps rpe -> BinopExpr bop (e rps rpe) (e2 rps rpe),
    \s e      rps rpe -> Forall s      (e rps rpe),
    \s e      rps rpe -> Exists s      (e rps rpe),
    \e        rps rpe -> SizeOf        (e rps rpe),
    \e e2 e3  rps rpe -> RepBy         (e rps rpe) (e2 rps rpe) (e3 rps rpe),
    \e e2 e3  rps rpe -> Cond          (e rps rpe) (e2 rps rpe) (e3 rps rpe),
    \e        rps rpe -> NewStore      (e rps rpe),
    \s        _   _   -> Dereference s )

exprRepbyAlgebra :: ExprAlgebra (String -> Expr -> Expr -> Expr)
exprRepbyAlgebra = (
    \s         _   _   _   -> Var  s,
    \i         _   _   _   -> LitI i,
    \b         _   _   _   -> LitB b,
    \          _   _   _   -> LitNull,
    \var index ars rpi rpe -> if (getVarName (var ars rpi rpe) == ars)
        then (Cond (BinopExpr Equal (index ars rpi rpe) (rpi)) (rpe) (ArrayElem (var ars rpi rpe) (index ars rpi rpe)))
        else (ArrayElem (var ars rpi rpe) (index ars rpi rpe)),
    \e         ars rpi rpe -> OpNeg         (e ars rpi rpe),
    \bop e e2  ars rpi rpe -> BinopExpr bop (e ars rpi rpe) (e2 ars rpi rpe),
    \s e       ars rpi rpe -> Forall s      (e ars rpi rpe),
    \s e       ars rpi rpe -> Exists s      (e ars rpi rpe),
    \e         ars rpi rpe -> SizeOf        (e ars rpi rpe),
    \e e2 e3   ars rpi rpe -> RepBy         (e ars rpi rpe) (e2 ars rpi rpe) (e3 ars rpi rpe),
    \e e2 e3   ars rpi rpe -> Cond          (e ars rpi rpe) (e2 ars rpi rpe) (e3 ars rpi rpe),
    \e         ars rpi rpe -> NewStore      (e ars rpi rpe),
    \s         _   _   _   -> Dereference s
    )
  where
    getVarName (Var s) = s
    getVarName _ = error "unreachable"
