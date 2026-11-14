#let title = [Expression rewriting as a proxy for bounded model checking]
#let authors = (
  ([Pepijn Bakker], [2024713]),
  ([Iris Scheepstra], [4454413]),
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

Despite this, execution time decreased only by a modest amount. Compared to a control with neither simplifier nor heuristic, the speedups we measured are small: mostly between 1× and 2×, with a maximum of about 2.9× (`pullUp`, $k = 40$). Individual settings are sometimes slower than the control. The combination that lets the simplifier's verdict bypass the solver is the most consistent, with a speedup in nearly all cases on `divByN` and `memberOf` (up to 1.8×), and it never loses more than about 15% on `pullUp`.

= Related Work
Symbolic execution and its main obstacle, path explosion, are surveyed by Baldoni et al. @baldoni2018symex. Bounded model checkers such as CBMC @clarke2004cbmc unroll the program up to a bound and pass the result to a SAT or SMT solver @biere1999bmc. We similarily unroll the program but use wlp to reason on each path in a backwards manner. The size of the resulting formulas is a known concern for w(l)p-based verifiers such as Boogie @barnett2005boogie, and Leino @leino2005efficientwp proposes a formulation that avoids exponential blow-up.

Most symbolic executors check feasibility eagerly. KLEE @cadar2008klee queries the solver at every branch, and relies on path-search heuristics, including depth-first search, to decide where to explore next. We instead decide whether or not to check at all, using a depth-based probabilistic measure.

KLEE rewrites expressions, splits constraints into independent sets and caches solutions @cadar2008klee in order to reduce the cost of solver queries. Empc @yao2025empc addresses the same problem from the other side, prioritising paths so that fewer infeasible ones are explored. Our simplifier (constant folding, absorption, deduplication and comparison merging) is most adjecent to the expression rewriting KLEE employs. Z3 @demoura2008z3 simplifies formulas itself, so our simplifier mainly saves the cost of calling the solver. This is in line with our measurements: since the solver already performs much of the same rewriting, the additional simplifier only yields a modest speedup, in contrast to the larger gains reported for KLEE's query optimisations @cadar2008klee.

= Implementation
// We do bounded model checker, early stopping if false
We implemented our bounded model checker in haskell. We bound by path length, which is calculated based on how many statements are within a path. // Mention if we do anything that makes what appears like 1 statement become multiple

// Use wlp
As described earlier on, we check paths using wlp. We also use wlp for checking if paths are feasible. // maybes explain more
To prevent any wlp from getting too long, paths are split on branching statements. We create a tree with this splitting and do the proper bounds checks. This is done lazily, so that we only perform these calculations on parts of the tree that are actually used.

// // Boolean formula's are first checked with simplifier and then z3,, using the bindings
The boolean formulas generated by the wlp throughout our program are always first checked with our own verifier. // if space elaborate a lot and add subheading
If this does not resolve to either true or false, then we check the simplified formula using z3. // Insert citation
For this we use the bindings created by // Insert citation
Within our program, we do not normally represent formulas in the datatypes introduced here, so we first always have a conversion step.

Our program has depth-based probabilistic feasibility checking, where deeper depths give lower probabilities. This is done, because the potential amount of paths being cut by discovering infeasibility decreases with depth. For the heuristic we use the following probability formula:
FORMULA HERE.
Q is a parameter we experiment with. To showcase what this formula does, here is the graph of probabilities given for k=constant:

Our program has a second heuristic option, called bypass. Turning this on means our simplifier always checks the feasibility of the program, and the probability only effects if z3 calls are made after this step.

// Mention what do if no feasible path found anywhere

= Methodology // Maybe there is a better name or something
// Here we explain the following probs:
// - Benchmark files used
The performance of our program is checked with 2 separate programs. The first only collects the runtimes. This is done with the criterion // cite
The second collects all the other statistics // list them
// Mention how many times we run

These parts are separate to ensure that the collection of these statistics does not influence the measured runtimes. To this extent, the reading and parsing of the tested files is also done before the benchmarking, and thus not a part of the measured runtimes.


= Experiment 1
Within experiment one, we aim to find the most performant heuristic. 
The first word within the name of a heuristic states whether the simplifier was active. The second part describes the heuristic used. Heuristic off means that feasibility was always checked. Heuristic on means it was on with Q=1, and (Q=16) is added if it was 16. Bypass means that the heuristic only decided whether to use z3, and a simple check with the simplifier was always done. The following section shows why we chose to use the specified Q-values.

== Finding the best Q value
In order to keep the figures readable and runtimes feasible, we need to determine which Q-values are used in the rest of the experiments. Therefore we plotted the mean execution time for 
k=30 of // fancy way to descrive series of Q used

// Insert figure of the Q
#include "exp1-q-plot.typ"

Here we can see that when looking at full simplifier with bypass, Q=16 gives anywhere from the same results, to a 10x improvement. Therefore this is one of the Q-values used in the other experiments.

However, when considering no simplifier, or simplifier without bypass, Q=16 worsens the outcomes for divByN.gcl. To ensure that these heuristics are fairly treated, Q=1 is also tested.

== Comparison of the execution time of each heuristic
// Intro time

#include "exp1-plot.typ"
// Graph

// Interpetation no possibles yet

== Number of feasibility checks performed

#include "feasibility-plot.typ"


// Graph

As can be seen in the figure, almost everything that can be pruned with divByN.gcl and memberOf.gcl, is pruned by the simplifier. For these files, calling z3 if the simplifier does not resolve to false is not worth it. However, this is not the case for pullUp.gcl. This is because our simplifier cannot handle some of the formulas created by pullUp, making it never able to simplify to true or false. 
// More conclusions 
// 



== Total length of formulas checked

#include "metrics-plot.typ"

// I say we drop the q graph here bc no interesting information

The figure plots the total length of the formulas checked by z3 for feasibility checks. Formula length is defined by the number of variables and literals in the formula. Our heuristic consistently checks a smaller length, with it decreasing with Q. However, this does not correlate with the execution time very well. The statistic does not include the z3 calls for the final wlp. Therefore the best possible outcome for this statistic would be to not check any feasibility, which quite obviously is not efficient.

// Does not have a super direct relationship with time, bc while we suspect checking these formulas to be a major contributer to the time, some formulas are checked way cheaper than others (which can also depend on simpy) 
// 

== Conclusion

= Experiment 2
Our second experiment checks our verifier's completeness. These tests are ran using our best heuristic, which we found to be ...

== Kill rate

== Performance
// To mention:
// - talk about performance
// - mention the kill rate

= Conclusion
// Simplification important, no matter if in z3 or locally
// Heuristic has big effect
// give our best combi
// second experiment talk


#bibliography("refs.bib", title: "References", style: "association-for-computing-machinery")
