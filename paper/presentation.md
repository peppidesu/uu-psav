---
theme: gaia
_class: lead
class: gaia
paginate: true
---
<style>
  :root {
    --color-background: #272e33;
    --color-foreground: #d3c6aa;
    --color-highlight: #67b8e3;
    --color-dimmed: #859289;
  }
  marp-pre {
      background: #2f383e;
      code {
          color: #859289;
      }
  }
</style>


# **Slides**

Subtitle


---

# Chain simplification + normalization


```hs
-- | Set-level normalisation of one maximal AND/OR chain
normChain :: Expr -> Maybe Expr
normChain e = case e of
  (_ :|| _) -> go (B True)  (B False) mkOr  orView  absorbOr  e
  (_ :&& _) -> go (B False) (B True)  mkAnd andView (collapseLowerBounds . absorbAnd) e
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
```
