{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Optimize where
import GCLParser.GCLDatatype (BinOp(..), Expr (..), opImplication, opAnd, opOr, Program (name))
import Types
import qualified GCLParser.PrettyPrint as G.PP
import Control.Monad
import qualified Data.Map.Strict as M
import Control.Lens.Plated (rewrite, rewriteM)
import Data.List

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

pattern I :: Int -> Expr
pattern I n = LitI n                    -- implicitly bidirectional

pattern B :: Bool -> Expr
pattern B b = LitB b

isLit :: Expr -> Bool
isLit (LitB _)  = True
isLit (LitI _)  = True
isLit LitNull   = True
isLit _         = False

-- | Operands of a maximal '||' chain, left to right.
orView :: Expr -> [Expr]
orView (e :|| e2) = orView e ++ orView e2
orView e          = [e]


-- | Operands of a maximal '&&' chain, left to right.
andView :: Expr -> [Expr]
andView (e :&& e2) = andView e ++ andView e2
andView e          = [e]

-- | Unfolds the '||' chain back into a tree (left-associative)
mkOr :: [Expr] -> Expr
mkOr []       = B False
mkOr [e]      = e
mkOr (e : es) = e :|| mkOr es

-- | Unfolds the '&&' chain back into a tree (left-associative)
mkAnd :: [Expr] -> Expr
mkAnd []       = B True
mkAnd [e]      = e
mkAnd (e : es) = e :&& mkAnd es

-- | Set-level normalisation of one maximal AND/OR chain
normChain :: Expr -> Maybe Expr
normChain e = case e of
  (_ :|| _) -> go (B True)  (B False) mkOr  orView  absorbOr  e
  (_ :&& _) -> go (B False) (B True)  mkAnd andView absorbAnd e
  _         -> Nothing
  where
    go ann unit mk view absorb x
      | ann `elem` xs = Just ann          -- annihilator present; x -> ann
      | y == x        = Nothing           -- x was already normal; report no change
      | otherwise     = Just y            -- x -> y
      where xs = view x
            y  = mk                  -- reconstruct tree
               $ absorb              -- apply absorption
               $ nub                 -- remove duplicate terms
               $ filter (/= unit) xs -- remove units

-- | Drop each disjunct that another disjunct implies by being its conjunct.
absorbOr :: [Expr] -> [Expr]
absorbOr ds = [ d | d <- ds, not $ any (`conjunctOf` d) ds  ]

-- | Drop each conjunct that another conjunct implies by being its disjunct.
absorbAnd :: [Expr] -> [Expr]
absorbAnd cs = [ c | c <- cs, not $ any (`disjunctOf` c) cs ]

-- "proper" = the test only applies when the *container* is that operator,
-- which makes these irreflexive: no node is a proper conjunct of itself.
conjunctOf :: Expr -> Expr -> Bool
conjunctOf c d@(_ :&& _) = c `elem` andView d
conjunctOf _ _ = False

disjunctOf :: Expr -> Expr -> Bool
disjunctOf c d@(_ :|| _) = c `elem` orView d
disjunctOf _ _ = False

truth :: Expr -> Maybe Bool
truth = \case
  LitB b                        -> Just b        -- used for recursive calls of `truth`

  -- comparison of literals
  I a :<  I b                   -> Just $ a <  b
  I a :<= I b                   -> Just $ a <= b
  I a :== I b                   -> Just $ a == b

  -- sizeof can never be negative
  SizeOf _ :<  I k | k <= 0     -> Just False
  SizeOf _ :<= I k | k <  0     -> Just False
  SizeOf _ :== I k | k <  0     -> Just False
  I k :<= SizeOf _ | k <= 0     -> Just True
  I k :<  SizeOf _ | k <  0     -> Just True
  I k :== SizeOf _ | k <  0     -> Just False

  OpNeg e                       -> not  <$> truth e
  e :&& e2                      -> (&&) <$> truth e <*> truth e2
  e :|| e2                      -> (||) <$> truth e <*> truth e2
  Forall _ e                    -> truth e
  Exists _ e                    -> truth e

  _                             -> Nothing

-- | One rewrite step. 'Nothing' means \"already normal at this node\".
step :: Expr -> Maybe Expr
step = \case
    -- trivial truths
    e | Just b <- truth e, e /= B b -> Just (B b)

    -- implication definition
    BinopExpr Implication e e2 -> Just (OpNeg e :|| e2)

    -- negation laws ----------------------------------------------------------
    OpNeg (OpNeg e)        -> Just e
    OpNeg (e :|| e2)       -> Just $ OpNeg e :&& OpNeg e2
    OpNeg (e :&& e2)       -> Just $ OpNeg e :|| OpNeg e2
    OpNeg (Forall s e)     -> Just $ Exists s (OpNeg e)
    OpNeg (Exists s e)     -> Just $ Forall s (OpNeg e)
    OpNeg (e :<= e2)       -> Just $ e2 :< e
    OpNeg (e :< e2)        -> Just $ e2 :<= e
    OpNeg (B True)      -> Just (B False)
    OpNeg (B False)     -> Just (B True)
    ---------------------------------------------------------------------------

    -- Swap comparison to always be < or <= -----------------------------------
    e :>= e2 -> Just (e2 :<= e)
    e :>  e2 -> Just (e2 :< e)
    ---------------------------------------------------------------------------

    -- conjunction / disjunction chain normalization --------------------------
    e | Just e' <- normChain e -> Just e'
    ---------------------------------------------------------------------------

    -- trivial conditions -----------------------------------------------------
    Cond c e _  | c == B True   -> Just e
    Cond c _ e2 | c == B False  -> Just e2
    ---------------------------------------------------------------------------

    -- inclusion/exclusion principle ------------------------------------------
    (e :< e2)  :|| (e3 :<= e4)  | e2 == e3 && e == e4  -> Just $ B True
    (e :<= e2) :|| (e3 :< e4)   | e2 == e3 && e == e4  -> Just $ B True
    (e :< e2)  :&& (e3 :<= e4)  | e2 == e3 && e == e4  -> Just $ B False
    (e :<= e2) :&& (e3 :< e4)   | e2 == e3 && e == e4  -> Just $ B False
    (e :<= e2) :&& (e3 :<= e4)  | e2 == e3 && e == e4  -> Just $ e :== e2
    (e :< e2)  :&& (e3 :<= e4)  | e == e3  && e2 == e4 -> Just $ e :< e2
    (e :<= e2) :&& (e3 :< e4)   | e == e3  && e2 == e4 -> Just $ e :< e2

    (I a :< e) :&& (I b :< e2)   | e == e2 -> Just $ I (max a b) :< e
    (I a :< e) :|| (I b :< e2)   | e == e2 -> Just $ I (min a b) :< e
    (I a :<= e) :&& (I b :<= e2) | e == e2 -> Just $ I (max a b) :<= e
    (I a :<= e) :|| (I b :<= e2) | e == e2 -> Just $ I (min a b) :<= e

    (e :- I a) :< e2  | e == e2 -> Just $ B (a > 0)
    (e :- I a) :<= e2 | e == e2 -> Just $ B (a >= 0)
    (e :- I a) :== e2 | e == e2 -> Just $ B (a == 0)
    (e :+ I a) :< e2  | e == e2 -> Just $ B (a < 0)
    (e :+ I a) :<= e2 | e == e2 -> Just $ B (a <= 0)
    (e :+ I a) :== e2 | e == e2 -> Just $ B (a == 0)

    (I a :<= e1) :&& (I b :<  e2) | e1 == e2 -> Just $ I (max (a - 1) b) :< e1
    (I a :<  e1) :&& (I b :<= e2) | e1 == e2 -> Just $ I (max a (b - 1)) :< e1
    (I a :<= e1) :|| (I b :<  e2) | e1 == e2 -> Just $ I (min (a - 1) b) :< e1
    (I a :<  e1) :|| (I b :<= e2) | e1 == e2 -> Just $ I (min a (b - 1)) :< e1

    (I a :<  e1) :&& (e2 :<  I b) | e1 == e2 && a + 1 >= b -> Just $ B False -- 4 < x && x < 5 is already impossible, but 3 < x && x < 5 is fine (x = 4)
    (I a :<= e1) :&& (e2 :<  I b) | e1 == e2 && a >= b     -> Just $ B False -- 4 <=x && x < 5 is possible, but 5 <= x && x < 5 isn't
    (I a :<  e1) :&& (e2 :<= I b) | e1 == e2 && a >= b     -> Just $ B False -- 4 < x && x <=5 is fine, 5 < x && x <= 5 is not
    (I a :<= e1) :&& (e2 :<= I b) | e1 == e2 && a > b      -> Just $ B False -- 4 <=x && x <=5 is fine, 4 <=x && x <= 4 is as well
    (I a :<  e1) :|| (e2 :<  I b) | e1 == e2 && a < b      -> Just $ B True  -- x > 3 || x < 4 is true for all x
    (I a :<= e1) :|| (e2 :<  I b) | e1 == e2 && a <= b     -> Just $ B True  -- x >= 4 || x < 4 is true for all x
    (I a :<  e1) :|| (e2 :<= I b) | e1 == e2 && a <= b     -> Just $ B True  -- x > 4 || x <= 4 is true for all x
    (I a :<= e1) :|| (e2 :<= I b) | e1 == e2 && a - 1 <= b -> Just $ B True  -- x >= 6 || x <= 5 is true for all x
    ---------------------------------------------------------------------------

    -- Integer constant folding -----------------------------------------------
    (e :+ I k) :<  I c -> Just (e :<  I (c - k))
    (e :- I k) :<  I c -> Just (e :<  I (c + k))
    (e :+ I k) :<= I c -> Just (e :<= I (c - k))
    (e :- I k) :<= I c -> Just (e :<= I (c + k))
    (e :+ I k) :== I c -> Just (e :== I (c - k))
    (e :- I k) :== I c -> Just (e :== I (c + k))
    I c :<  (e :+ I k) -> Just (I (c - k) :<  e)
    I c :<  (e :- I k) -> Just (I (c + k) :<  e)
    I c :<= (e :+ I k) -> Just (I (c - k) :<= e)
    I c :<= (e :- I k) -> Just (I (c + k) :<= e)
    I c :== (e :+ I k) -> Just (I (c - k) :== e)
    I c :== (e :- I k) -> Just (I (c + k) :== e)

    (I a) :+ (I 0)      -> Just $ I a
    (I 0) :+ (I b)      -> Just $ I b
    (I a) :+ (I b)      -> Just $ I (a + b)
    (e :+ I a) :+ (I b) -> Just $ e :+ I (a + b)
    (I a :+ e) :+ (I b) -> Just $ e :+ I (a + b)
    (e :- I a) :+ (I b)
        | a == b    -> Just e
        | a >  b    -> Just $ e :- I (a - b)
        | otherwise -> Just $ e :+ I (b - a)

    (I a) :- (I 0)      -> Just $ I a
    (I 0) :- (I a)      -> Just $ I (-a)
    (I a) :- (I b)      -> Just $ I (a - b)
    (e :- I a) :- (I b) -> Just $ e :- I (a + b)
    (I a :- e) :- (I b) -> Just $ I (a - b) :- e
    (e :+ I a) :- (I b)
      | a == b    -> Just e
      | a <  b    -> Just $ e :- I (b - a)
      | otherwise -> Just $ e :+ I (a - b)

    (I _) :* (I 0)      -> Just $ I 0
    (I a) :* (I 1)      -> Just $ I a
    (I 0) :* (I _)      -> Just $ I 0
    (I 1) :* (I b)      -> Just $ I b
    (I a) :* (I b)      -> Just $ I (a * b)
    (I a :* e) :* (I b) -> Just $ e :* I (a * b)
    (e :* I a) :* (I b) -> Just $ e :* I (a * b)

    (I a) :/ (I 1)      -> Just $ I a
    (I 0) :/ (I _)      -> Just $ I 0
    (I a) :/ (I b)      | b /= 0 -> Just $ I (div a b)
    (e :/ I a) :/ (I b) | b /= 0 -> Just $ e :/ I (a * b)
    (I a :/ e) :/ (I b) | b /= 0 -> Just $ I (div a b) :/ e

    (I a) :< (I b)      -> Just $ B (a <  b)
    (I a) :<= (I b)     -> Just $ B (a <= b)
    ---------------------------------------------------------------------------

    Parens e -> Just e  -- remove parens as they are just parser glue and the AST shape ensures correct evaluation order

    _ -> Nothing

optimize :: Expr -> Expr
optimize = rewrite step

-- TODO: dealing with minus in LitI (is this necessary?)
-- TODO: consider simplifying not (a or b) to (not a) and (not b)

--------------------------------------------------------------------------------
