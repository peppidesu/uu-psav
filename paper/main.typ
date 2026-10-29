#import "@preview/clean-acmart:0.0.1": acmart, acmart-ccs, acmart-keywords, acmart-ref, to-string
#import "@preview/lilaq:0.6.0" as lq
#import "@preview/cetz:0.5.2"
#import "@preview/cetz-plot:0.1.4"

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
#let bench1 = csv("../graphs/benchExperiment1.csv", row-type: dictionary)
#let bench2 = csv("../graphs/benchExperiment1.csv", row-type: dictionary)

#let filterFile(fileName, dats) = {dats.filter(x => x.file == fileName)}
#let filterCfg(cfg, dats) = {dats.filter(x => x.settings == cfg)}
#let filterQ(val, dats) = {dats.filter(x => int(x.Q) == val)}

#let getNMean(col) = {col.map(x=> (float(x.N), float(x.mean)))}
#let getKMean(col) = {col.map(x=> (float(x.K), float(x.mean)))}
#let getNMeanLB(col) = {col.map(x=> (float(x.N), float(x.meanLB)))}
#let getKMeanLB(col) = {col.map(x=> (float(x.K), float(x.meanLB)))}
#let getKMeanUB(col) = {col.map(x=> (float(x.K), float(x.meanUB)))}

Experiment 1, mean time, ran on divByN
#cetz.canvas({
  import cetz.draw: *
  import cetz-plot: *

  let getMainData(settings, val) = {getKMean(filterCfg(settings, filterQ(val, filterFile("divByN", bench1))))}
  let getLower(settings, val) = {getKMeanLB(filterCfg(settings, filterQ(val, filterFile("divByN", bench1))))}
  let getUpper(settings, val) = {getKMeanUB(filterCfg(settings, filterQ(val, filterFile("divByN", bench1))))}
  plot.plot(
    size: (7, 7),
    x-label: "N",
    y-label: "Mean",
    {
      // This will be done for each heuristic used
        plot.add(
          getMainData("default", 1),
          line: "linear",
          label: "Q=1",
        )
        plot.add-fill-between(
          getUpper("default", 1),
          getLower("default", 1),
          line: "linear"
        )
        plot.add(
          getMainData("nosimp", 1),
          line: "linear",
          label: "Q=1",
        )
        plot.add-fill-between(
          getUpper("nosimp", 1),
          getLower("nosimp", 1),
          line: "linear"
        )
    }
  )
})

=== Feasibility Heuristic

=== Simplifier


== Results

= Acknowledgement
#lorem(20)

#bibliography("refs.bib", title: "References", style: "association-for-computing-machinery")

#colbreak(weak: true)
#set heading(numbering: "A.a.a")

= Artifact Appendix
