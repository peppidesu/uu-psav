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


# **INFOPSV - GCL Bounded symbolic checker**

Pepijn Bakker - Iris Scheepstra - Konstantinos Tsikouris


---

# **Contents**
- General structure
- Simplifier
- Heuristic
- Results
- Conclusion


# **General structure**
- Unrolling branches into lazy tree
- Truncate to max path length (k)
- Depth first exploration (favor staying in loops)

# **General structure - WLP + Feasibility**
- WLP are used to check if a program is correct.
- Feasibility checking is done by calculating the wlp up to that point.
  - Simplifier first
  - Then Z3

# **General structure - Z3**
<!-- I say we skip, not that interesting?-->

# **Simplifier**
- Constant folding, some examples:
  - not true = false
  - true && true = true
  - Integer folding limited, e.g. 4 < 3 or 3 == 3
- Chain simplification + normalization
  - Inspects maximal and/or chains



---

# Chain simplification + normalization
- Absorption:
  - a || (a && b) = a
  - (a || b) && a = a
- Deduplication:
  - a || b || a = a || b
  - a && b && a = a
- Unit removal:
  - a || false = a
  - a && true  = a
- Annihilation: 
  - a || true  = true
  - a && false = false
- Comparison merging:
  - a < 4 && a > 5 = false
  - a < 4 && a < 5 = a < 4

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
--- 
# Heuristic
- Simplifier off, simplifier fully on
<!-- mention the partial capability, although not in use--->
- Probability based heuristic
  - Fully on, fully off, and possibility to always check with at least the simplifier.
- Deeper = lower chance
- Q value adjust how fast this drop off happens

---
# Results - first experiment
<!-- Here we can see the mean execution times on 3 of the benchmarks-->
![Alt text](Screenshot-2026-11-04-093303.png)

- For these benchmarks, there is a clear winner:
Full simplifier, heuristics on with Q=16, and always feasibility 
checking with at least the simplifier.
- Notice how pullUp has less difference between them.
- Comparison of full vs no simplifier

---
# Results - effect of Q on heuristic
![Alt text](Screenshot-2026-11-04-094019.png)
- Clear winner once more
- Increasing Q has opposite effects in divByN
  - Showcases importance of the simplifier
- pullUp again has very similar performance

--- 
# Results - number of feasibility checks needed
![Alt text](Screenshot-2026-11-04-094230.png)
- 

--- 
# Results - number of feasibility checks needed
![Alt text](Screenshot-2026-11-04-095631.png)
- 

---
# Possible improvements and future research
- Depth first vs breadth first, and if there is a smart way to figure out which dynamically.
- Improve simplifier, with some form of caching
- Dynamically find a way to choose whether to rely only on the simplifier.
- Expand the expression data type to capture more synthetic expressions such as summation of linear terms and use more efficient solvers as the backend.
