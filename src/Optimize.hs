module Optimize where
import GCLParser.GCLDatatype (BinOp(..), Expr (..), opImplication, opAnd, opOr)
import Types
import WLP (foldExpr)
import qualified GCLParser.PrettyPrint as G.PP
import Control.Monad

exprVerbEqAlgebra :: ExprAlgebra Expr
exprVerbEqAlgebra = let
    verbatimEquality bop e e2 | bop `elem` [Equal, GreaterThanEqual, LessThanEqual] && e == e2 = LitB True
                              | bop == Equal && isLit e && isLit e2                 && e /= e2 = LitB False
                              | bop `elem` [LessThan, GreaterThan]                  && e == e2 = LitB False
                              | otherwise = BinopExpr bop e e2
    isLit (LitB _)  = True
    isLit (LitI _)  = True
    isLit (LitNull) = True
    isLit _         = False
 in (
    \s        -> Var s,
    \i        -> LitI i,
    \b        -> LitB b,
    LitNull,
    \e e2     -> ArrayElem e e2,
    \e        -> OpNeg     e,
    verbatimEquality,
    \s e      -> Forall s e,
    \s e      -> Exists s e,
    \e        -> SizeOf e,
    \e e2 e3  -> RepBy e e2 e3,
    \e e2 e3  -> Cond  e e2 e3,
    \e        -> NewStore e,
    \s        -> Dereference s )

--- optimizations

exprTautologyAlgebra :: ExprAlgebra Expr
exprTautologyAlgebra = (
    \s        -> Var s,
    \i        -> LitI i,
    \b        -> LitB b,
    LitNull,
    \e e2     -> ArrayElem e e2,
    opneg,
    tautology,
    \s e      -> Forall s e,
    \s e      -> Exists s e,
    \e        -> SizeOf e,
    \e e2 e3  -> RepBy e e2 e3,
    \e e2 e3  -> cond    e e2 e3,
    \e        -> NewStore e,
    \s        -> Dereference s )
  where
    tautology And e e2  | e == LitB (False) || e2 == LitB (False) = LitB (False)
                        | e == LitB (True)                        = e2
                        | e2 == LitB (True)                       = e
                        | otherwise = opAnd e e2
    tautology Or  e e2  | e == LitB (True) || e2 == LitB (True) = LitB (True)
                        | e == LitB (False)                     = e2
                        | e2 == LitB (False)                    = e
                        | otherwise = opOr     e e2
    tautology Implication e e2 | e == LitB (False)                        = LitB (True)
                                | e2 == LitB (True)                        = LitB (True)
                                | e  == LitB (True) && e2 == LitB (False)  = LitB (False)
                                | e2 == LitB (False)                       = OpNeg e
                                | otherwise                                = opImplication e e2
    tautology bop e e2 = BinopExpr bop e e2

    cond (LitB True)  e _    = e
    cond (LitB False) _ e2   = e2
    cond g              e e2 = Cond g e e2
    opneg (LitB True)        = LitB False
    opneg (LitB False)       = LitB True
    opneg e                  = OpNeg e

exprIntCalcAlgebra :: ExprAlgebra Expr
exprIntCalcAlgebra = (
    \s        -> Var s,
    \i        -> LitI i,
    \b        -> LitB b,
                 LitNull,
    \e e2     -> ArrayElem e e2,
    \e        -> OpNeg e,
    \bop e e2 -> calc bop e e2,
    \s e      -> Forall s e,
    \s e      -> Exists s e,
    \e        -> SizeOf e,
    \e e2 e3  -> RepBy e e2 e3,
    \e e2 e3  -> Cond  e e2 e3,
    \e        -> NewStore e,
    \s        -> Dereference s )
  where
    calc Plus     (LitI a) (LitI b)                       = LitI (a + b)
    calc Plus     ((BinopExpr Plus  e (LitI a))) (LitI b) = BinopExpr Plus e (LitI (a + b))
    calc Plus     ((BinopExpr Minus e (LitI a))) (LitI b) | a == b    = e
                                                        | a > b     = BinopExpr Minus e (LitI (a - b))
                                                        | otherwise = BinopExpr Plus  e (LitI (b - a))

    calc Minus    (LitI a) (LitI b)                       = LitI (a - b)
    calc Minus    ((BinopExpr Minus e (LitI a))) (LitI b) = BinopExpr Minus e (LitI (a + b))
    calc Minus    ((BinopExpr Plus  e (LitI a))) (LitI b) | a == b    = e
                                                        | a < b     = BinopExpr Minus e (LitI (b - a))
                                                        | otherwise = BinopExpr Plus  e (LitI (a - b))

    calc Multiply (LitI a) (LitI b)                          = LitI (a * b)
    calc Multiply ((BinopExpr Multiply (LitI a) e)) (LitI b) = BinopExpr Multiply e (LitI (a * b))
    calc Multiply ((BinopExpr Multiply e (LitI a))) (LitI b) = BinopExpr Multiply e (LitI (a * b))

    calc Divide   (LitI a) (LitI b)                          = LitI (div a b)
    calc Divide   ((BinopExpr Divide   e (LitI a))) (LitI b) = BinopExpr Divide   e (LitI (a * b))
    calc Divide   ((BinopExpr Divide   (LitI a) e)) (LitI b) = BinopExpr Divide   e (LitI (a * b))

    calc LessThan         (LitI a) (LitI b) = LitB (a < b)
    calc LessThanEqual    (LitI a) (LitI b) = LitB (a <= b)
    calc GreaterThan      (LitI a) (LitI b) = LitB (a > b)
    calc GreaterThanEqual (LitI a) (LitI b) = LitB (a >= b)

    calc bop e e2 = BinopExpr bop e e2

exprRewriteAlgebra :: ExprAlgebra Expr
exprRewriteAlgebra = (
    \s        -> Var s,
    \i        -> LitI i,
    \b        -> LitB b,
                 LitNull,
    \e e2     -> ArrayElem e e2,
    removeDoubleNeg,
    rewriteImplication,
    \s e      -> Forall s e,
    \s e      -> Exists s e,
    \e        -> SizeOf e,
    \e e2 e3  -> RepBy e e2 e3,
    \e e2 e3  -> Cond  e e2 e3,
    \e        -> NewStore e,
    \s        -> Dereference s )
    where
      rewriteImplication Implication e e2   = opOr (OpNeg e) (e2)
      rewriteImplication bop           e e2 = BinopExpr bop e e2
      removeDoubleNeg (OpNeg e)             = e
      removeDoubleNeg e                     = OpNeg e

optimizeUntilIdempotence :: Expr -> Expr
optimizeUntilIdempotence exprs = let
    optimized = foldr1 (.) (map foldExpr [
        exprVerbEqAlgebra,
        exprTautologyAlgebra,
        exprIntCalcAlgebra,
        exprRewriteAlgebra
     ]) exprs;
 in if optimized == exprs then exprs else optimizeUntilIdempotence optimized

optimizeTrace :: (Expr, Int) -> IO ()
optimizeTrace (exprs, index) = let
    optimized = foldr1 (.) (map foldExpr [
        exprVerbEqAlgebra,
        exprTautologyAlgebra,
        exprIntCalcAlgebra,
        exprRewriteAlgebra
     ]) exprs;
 in do
  _ <- putStrLn $ show $ G.PP.ppExpr exprs
  _ <- putStrLn (show index ++"-----")
  when (optimized /= exprs) $ optimizeTrace (optimized, index)

-- TODO: dealing with minus in LitI (is this necessary?)
-- TODO: consider simplifying not (a or b) to (not a) and (not b)

--------------------------------------------------------------------------------
