module SMT (
CounterExample,
dummySat
)
where

-- For each var, add a value it needs to get a counterexample
type CounterExample = [(String, String)] 
dummySat :: Expr -> IO Maybe CounterExample
dummySat _ = return Nothing
