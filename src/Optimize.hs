{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Optimize where
import GCLParser.GCLDatatype (BinOp(..), Expr (..), opImplication, opAnd, opOr, Program (name))
import Types
import qualified GCLParser.PrettyPrint as G.PP
import Control.Monad
import qualified Data.Map.Strict as M

exprVerbEqAlgebra :: ExprAlgebra Expr
exprVerbEqAlgebra n@(BinopExprF bop e e2)
    | bop `elem` [Equal, GreaterThanEqual, LessThanEqual] && e == e2 = LitB True
    | bop == Equal && isLit e && isLit e2                 && e /= e2 = LitB False
    | bop `elem` [LessThan, GreaterThan]                  && e == e2 = LitB False
    | otherwise = embed n
  where
    isLit (LitB _)  = True
    isLit (LitI _)  = True
    isLit (LitNull) = True
    isLit _         = False
exprVerbEqAlgebra n = embed n


exprBoolCalcAlgebra :: ExprAlgebra Expr
exprBoolCalcAlgebra n@(BinopExprF And e e2)
    | e == LitB (False) || e2 == LitB (False) = LitB (False)
    | e == LitB (True)                        = e2
    | e2 == LitB (True)                       = e
    | otherwise                               = embed n
exprBoolCalcAlgebra n@(BinopExprF Or e e2)
    | e == LitB (True) || e2 == LitB (True)   = LitB (True)
    | e == LitB (False)                       = e2
    | e2 == LitB (False)                      = e
    | otherwise                               = embed n
exprBoolCalcAlgebra n@(BinopExprF Implication e e2)
    | e == LitB (False)                        = LitB (True)
    | e2 == LitB (True)                        = LitB (True)
    | e  == LitB (True) && e2 == LitB (False)  = LitB (False)
    | e2 == LitB (False)                       = OpNeg e
    | otherwise                                = embed n
exprBoolCalcAlgebra n@(CondF c e e2)
    | c == LitB True  = e
    | c == LitB False = e2
    | otherwise       = embed n
exprBoolCalcAlgebra n@(OpNegF e)
    | e == LitB True  = LitB False
    | e == LitB False = LitB True
    | otherwise       = embed n

exprBoolCalcAlgebra n = embed n

pattern I :: Int -> Expr
pattern I n = LitI n                    -- implicitly bidirectional

pattern B :: Bool -> Expr
pattern B b = LitB b

infixl 6 :+
infixl 6 :-
infixl 7 :*
infixl 7 :/
infix  5 :>=
infix  5 :<=
infix  5 :>
infix  5 :<
infix  5 :==

pattern (:+) :: Expr -> Expr -> Expr
pattern e :+ e2 <- BinopExpr Plus e e2 where
  e :+ e2 = BinopExpr Plus e e2

pattern (:-) :: Expr -> Expr -> Expr
pattern e :- e2 <- BinopExpr Minus e e2 where
  e :- e2 = BinopExpr Minus e e2

pattern (:*) :: Expr -> Expr -> Expr
pattern e :* e2 <- BinopExpr Multiply e e2 where
  e :* e2 = BinopExpr Multiply e e2

pattern (:/) :: Expr -> Expr -> Expr
pattern e :/ e2 <- BinopExpr Divide e e2 where
  e :/ e2 = BinopExpr Divide e e2

pattern (:<=) :: Expr -> Expr -> Expr
pattern e :<= e2 <- BinopExpr LessThanEqual e e2 where
  e :<= e2 = BinopExpr LessThanEqual e e2

pattern (:>=) :: Expr -> Expr -> Expr
pattern e :>= e2 <- BinopExpr GreaterThanEqual e e2 where
  e :>= e2 = BinopExpr GreaterThanEqual e e2

pattern (:<) :: Expr -> Expr -> Expr
pattern e :< e2 <- BinopExpr LessThan e e2 where
  e :< e2 = BinopExpr LessThan e e2

pattern (:>) :: Expr -> Expr -> Expr
pattern e :> e2 <- BinopExpr GreaterThan e e2 where
  e :> e2 = BinopExpr GreaterThan e e2

pattern (:==) :: Expr -> Expr -> Expr
pattern e :== e2 <- BinopExpr Equal e e2 where
  e :== e2 = BinopExpr Equal e e2

pattern (:||) :: Expr -> Expr -> Expr
pattern e :|| e2 <- BinopExpr Or e e2 where
  e :|| e2 = BinopExpr Or e e2

pattern (:&&) :: Expr -> Expr -> Expr
pattern e :&& e2 <- BinopExpr And e e2 where
  e :&& e2 = BinopExpr And e e2

exprIntCalcAlgebra :: ExprAlgebra Expr
exprIntCalcAlgebra (BinopExprF bop' e' e2') = calc bop' e' e2'
  where
    calc :: BinOp -> Expr -> Expr -> Expr

    calc Plus     (I a)      (I 0) = I a
    calc Plus     (I 0)      (I b) = I b
    calc Plus     (I a)      (I b) = I (a + b)
    calc Plus     (e :+ I a) (I b) = e :+ I (a + b)
    calc Plus     (I a :+ e) (I b) = e :+ I (a + b)
    calc Plus     (e :- I a) (I b)
        | a == b    = e
        | a >  b    = e :- I (a - b)
        | otherwise = e :+ I (b - a)

    calc Minus    (I a)      (I 0) = I a
    calc Minus    (I 0)      (I a) = I (-a)
    calc Minus    (I a)      (I b) = I (a - b)
    calc Minus    (e :- I a) (I b) = e :- I (a + b)
    calc Minus    (I a :- e) (I b) = I (a - b) :- e
    calc Minus    (e :+ I a) (I b)
        | a == b    = e
        | a <  b    = e :- I (b - a)
        | otherwise = e :+ I (a - b)

    calc Multiply (I _)      (I 0) = I 0
    calc Multiply (I a)      (I 1) = I a
    calc Multiply (I 0)      (I _) = I 0
    calc Multiply (I 1)      (I b) = I b
    calc Multiply (I a)      (I b) = I (a * b)
    calc Multiply (I a :* e) (I b) = e :* I (a * b)
    calc Multiply (e :* I a) (I b) = e :* I (a * b)

    calc Divide   (I a)      (I 1) = I a
    calc Divide   (I 0)      (I _) = I 0
    calc Divide   (I a)      (I b) = I (div a b)
    calc Divide   (e :/ I a) (I b) = e :/ I (a * b)
    calc Divide   (I a :/ e) (I b) = I (div a b) :/ e

    calc LessThan         (I a) (I b) = B (a <  b)
    calc LessThanEqual    (I a) (I b) = B (a <= b)

    calc bop e e2 = BinopExpr bop e e2

exprIntCalcAlgebra n = embed n

exprFlipAlgebra :: ExprAlgebra Expr
exprFlipAlgebra (BinopExprF GreaterThanEqual e e2) = e2 :<= e
exprFlipAlgebra (BinopExprF GreaterThan e e2) = e2 :< e
exprFlipAlgebra n = embed n

exprDedupAlgebra :: ExprAlgebra Expr
exprDedupAlgebra (BinopExprF And e e2) | e == e2 = e
exprDedupAlgebra (BinopExprF Or e e2)  | e == e2 = e
exprDedupAlgebra n = embed n

exprRewriteAlgebra :: ExprAlgebra Expr
exprRewriteAlgebra (BinopExprF Implication e e2) = opOr (OpNeg e) (e2)
exprRewriteAlgebra (OpNegF (OpNeg e))            = e
exprRewriteAlgebra n                             = embed n

optimizeUntilIdempotence :: Expr -> Expr
optimizeUntilIdempotence exprs = let
    optimized = foldr1 (.) (map foldExpr [
        exprRewriteAlgebra,
        exprDedupAlgebra,
        exprVerbEqAlgebra,
        exprBoolCalcAlgebra,
        exprIntCalcAlgebra
     ]) exprs;
 in if optimized == exprs then exprs else optimizeUntilIdempotence optimized

optimizeTrace :: (Expr, Int) -> IO ()
optimizeTrace (exprs, index) = let
    optimized = foldr1 (.) (map foldExpr [
        exprVerbEqAlgebra,
        exprBoolCalcAlgebra,
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
