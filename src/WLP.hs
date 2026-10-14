{-# LANGUAGE LambdaCase #-}
module WLP where
import Types
import GCLParser.GCLDatatype (Expr(..), BinOp (..), opAnd, opImplication)

wlp' :: Stmt -> Expr -> Expr
wlp'  Skip            = id
wlp' (Assert    e)    = opAnd e
wlp' (Assume    e)    = opImplication e
wlp' (Assign  s e)    = foldExpr (substAlg s e)
wlp' (AAssign s e e2) = foldExpr (repbyAlg s e e2)

substAlg :: String -> Expr -> ExprAlgebra Expr
substAlg rps rpe = \case
    VarF s | s == rps -> rpe
    n                 -> embed n

repbyAlg :: String -> Expr -> Expr -> ExprAlgebra Expr
repbyAlg ars rpi rpe = \case
    ArrayElemF v i | Var s <- v, s == ars -> Cond (BinopExpr Equal i rpi) rpe (ArrayElem v i)
    n                                     -> embed n
