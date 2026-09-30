# uu-psav

## setup

Make sure to run
```
git submodule update --init
```
after cloning to pull gclparser as a submodule.

testing if this works :)

## Still todo:
- Introduce heuristics for when to call z3 in the feasibility checks. (keep in mind:
((~(x == null) && ~(y == null)) && (x == y))
as a condition (reftest.gcl))
  - Always check
  - Never check
  - Check some small amount of possible values, if valid you do not need to call z3
    - Do this with random numbers
    - Or maybe numbers really close to existing numbers
    - Maybe do here if there is a x == y, replace all y with x?
- Feasibility checking expression simplification before calling z3
- Expanding simplification 
  - Forall and Exists currently have nothing
  - NegOp (some numeric binary expression) simplify to remove the negOp
  - Consider if we want to simplify the things created by a normal counter loop away (things like (1 < #a) && (2 < # a) && (3<#a) etc)
  - Check if more is needed
!- Z3 conversion and running.
!- Local variables
- Change output created by running (requires changing the feasibility function to keep 2 counters and 
wlp needs to return invalid paths):
  - Show the reject + full path or accept as in the project document
  - Report the statistics
- Do the paper
- Other things
  - Like the optional parts or more optimizations
