#import "@preview/clean-acmart:0.0.1": acmart, acmart-ccs, acmart-keywords, acmart-ref, to-string

#let title = [
  Insert Your Title Here∗
]
#let authors = (
  // You can use grouped affiliations with mark
  (
    name: [Pepijn Bakker],
    email: "p.bakker6@students.uu.nl",
  ),
  (
    // Should I use string or content? It doesn't matter
    name: [Iris Scheepstra],
    email: "email1@email.com",
  ),
  (
    // Should I use string or content? It doesn't matter
    name: [Konstantinos Tsikouris],
    email: "email2@email.com",
  ),
)
#let affiliations = (
  (
    name: [Utrecht University],
    department: [Faculty of Science],
  ),
)

#let conference = (
  name:  [],
  short: [],
  year:  [],
  date:  [],
  venue: [],
)
#let doi = " "
#let ccs = (
  (
    generic: [Software and its engineering],
    specific: ([Virtual machines], [Virtual memory], ),
  ),
  (
    generic: [Computer systems organization],
    specific: ([Heterogeneous (hybrid) systems], ),
  )
)

#let ccs = ()
#let keywords = ()

#show: acmart.with(
  title: title,
  authors: authors,
  affiliations: affiliations,
  conference: conference,
  doi: doi,
  copyright: [],
  // Set review to submission ID for the review process or to "none" for the final version.
  // review: [\#001],
)


#show heading: set block(above: 1em)

= Abstract
The process of scientific writing is often tangled up with the intricacies of typesetting, leading to frustration and wasted time for researchers. In this paper, we introduce Typst, a new typesetting system designed specifically for scientific writing.
Typst untangles the typesetting process, allowing researchers to compose papers faster. In a series of experiments we demonstrate that Typst offers several advantages, including faster document creation, simplified syntax, and increased ease-of-use.

#acmart-ccs(ccs)
#acmart-keywords(keywords)

== Introduction

== Background

== Methods

=== Feasibility Heuristic

=== Simplifier


== Results

= Acknowledgement
#lorem(20)

#bibliography("refs.bib", title: "References", style: "association-for-computing-machinery")

#colbreak(weak: true)
#set heading(numbering: "A.a.a")

= Artifact Appendix
