#let title = [Expression rewriting as a proxy for bounded model checking]
#let authors = (
  ([Pepijn Bakker], [2024713]),
  ([Iris Scheepstra], [0000000]),
  ([Konstantinos Tsikouris], [0000000]),
)
// ACM sigconf-style layout (two columns, US Letter), without conference metadata.
#set page(
  paper: "us-letter",
  margin: (x: 0.75in, top: 1in, bottom: 1in),
  columns: 2,
  numbering: "1",
)
#set columns(gutter: 0.33in)
#set text(font: "Libertinus Serif", size: 9pt)
#set par(justify: true, leading: 0.55em, spacing: 0.55em, first-line-indent: 1em)
#set heading(numbering: "1.1")
#show heading: set text(weight: "bold")
#show heading.where(level: 1): it => {
  set text(size: 10pt)
  block(above: 1.4em, below: 0.8em, upper(it))
}
#show heading.where(level: 2): it => block(above: 1.2em, below: 0.7em, text(size: 9pt, it))
#show figure.caption: set text(size: 8pt)
#show bibliography: set text(size: 8pt)

// Title block spanning both columns.
#place(
  top + center,
  scope: "parent",
  float: true,
  {
    set align(center)
    set par(first-line-indent: 0pt)
    text(size: 17pt, weight: "bold", title)
    v(1.2em)
    grid(
      columns: authors.len() * (1fr,),
      ..authors.map(a => text(size: 11pt)[#a.at(0) \ #a.at(1)]),
    )
    v(0.8em)
    text(size: 10pt)[Utrecht University \ #datetime.today().display("[month repr:long] [day], [year]")]
    v(1.5em)
  },
)

#set par(first-line-indent: 0pt)
*Abstract.* Abstract goes here.

#set par(first-line-indent: 1em)

= Introduction
As part of the Program Semantics and Verification course, we were tasked to write a wlp#footnote[weakest liberal precondition]-based bounded symbolic verification tool for a variation of the GCL language. That is, a tool that can verify whether or not a program satisifies a formal specification, usually in the form of a pre- and post-condition. This particular kind of verifier does this by exploring all possible branches up to a certain bound (hence 'bounded'). Then for each path, it finds the weakest liberal pre-condition, a formula that holds for initial values where that path will terminate in the post-condition, if it terminates. Finally, it checks if the path is valid by solving for the negation of this formula, using SAT or SMT.

Verification of programs, even for simple languages, is challenging due to the computational complexity involved: the number of paths to explore explodes as the search depth increases @baldoni2018symex. Even worse, many of these generated paths cannot logically be reached given any set of initial values (That is, they are _infeasible_). It turns out that the wlp of an infeasible path is always trivially true, so checking these paths is a significant waste of computation @yao2025empc.

Pruning infeasible paths is therefore an important step in the verification process. However, doing so is costly in and of itself, and will be for naught in cases where the path turns out to be feasible after all. As a result, coming up with a good heuristic for feasibility is an important part of this assignment. Another challenge is the fact that solver queries themselves are quite expensive, especially for longer formulas @leino2005efficientwp.

For our implementation, we were interested to see how extensive simplification could reduce the overhead of feasibility checking. We also explored a probabilistic method of reducing feasibility checking, based on the exploration depth. During implementation and early testing, we found out that the simplifier alone was able to deem many paths infeasible, and this led to the question of whether the simplifier itself could be a proxy for actual solver queries.

The results showed that in two out of three test programs, the simplifier was able to reason away nearly all infeasible paths. For the third program however, all the pruning was attributed to the solver instead.

= Related Work
Symbolic execution and its main obstacle, path explosion, are surveyed by Baldoni et al. @baldoni2018symex. Bounded model checkers such as CBMC @clarke2004cbmc unroll the program up to a bound and pass the result to a SAT or SMT solver @biere1999bmc. We similarily unroll the program but use wlp to reason on each path in a backwards manner. The size of the resulting formulas is a known concern for w(l)p-based verifiers such as Boogie @barnett2005boogie, and Leino @leino2005efficientwp proposes a formulation that avoids exponential blow-up.

Most symbolic executors check feasibility eagerly. KLEE @cadar2008klee queries the solver at every branch, and relies on path-search heuristics, including depth-first search, to decide where to explore next. We instead decide whether or not to check at all, using a depth-based probabilistic measure.

KLEE rewrites expressions, splits constraints into independent sets and caches solutions @cadar2008klee in order to reduce the cost of solver queries. Empc @yao2025empc addresses the same problem from the other side, prioritising paths so that fewer infeasible ones are explored. Our simplifier (constant folding, absorption, deduplication and comparison merging) is most adjecent to the expression rewriting KLEE employs. Z3 @demoura2008z3 simplifies formulas itself, so our simplifier mainly saves the cost of calling the solver.



#bibliography("refs.bib", title: "References", style: "association-for-computing-machinery")
