#import "@preview/cetz:0.4.2"
#import "@preview/cetz-plot:0.1.3"

// Execution time vs. Q for benchExperiment1 (K = 30).
//
// Encoding per setting (one colour each):
//   line + marker   Mean
//   shaded band     Mean ± Stddev
//
// Q values (0,1,2,4,8,16) are placed at equidistant positions.
// The y axis is logarithmic (values are log10-transformed by hand, since
// cetz-plot has no log axes). Small x offsets keep the settings from overlapping.

#let raw-rows = csv("./benchExperiment1.csv", row-type: dictionary)

// The Q sweep uses K = 30 for every file; K and Q are compared numerically.
// Later rows win on duplicates.
#let k-fixed = 30
#let qs = (0, 1, 2, 4, 8, 16)
#let files = ("divByN.gcl", "memberOf.gcl", "pullUp.gcl")

#let settings = (
  ("perf_simplifier-none_heuristic-on-bypass", "none, heuristic on", rgb("#d55e00")),
  ("perf_simplifier-full_heuristic-on", "full, heuristic on", rgb("#0072b2")),
  ("perf_simplifier-full_heuristic-on-bypass", "full, heuristic on + bypass", rgb("#cc79a7")),
)

#let floor-val = 1e-3
#let lg(x) = calc.log(calc.max(x, floor-val), base: 10)

// Rows for one file/setting as dictionaries sorted by Q; x is the position index.
#let series(file, setting) = {
  let by-q = (:)
  for r in raw-rows {
    if r.File.ends-with("/" + file) and r.Settings == setting and float(r.K) == k-fixed {
      by-q.insert(str(float(r.Q)), r)
    }
  }
  qs.enumerate().filter(((i, q)) => str(float(q)) in by-q).map(((i, q)) => {
    let r = by-q.at(str(float(q)))
    (
      x: i,
      m: float(r.Mean), mlb: float(r.MeanLB), mub: float(r.MeanUB),
      s: float(r.Stddev), slb: float(r.StddevLB), sub: float(r.StddevUB),
    )
  })
}

#let y-ticks = (
  (-3, [1 ms]), (-2, [10 ms]), (-1, [100 ms]), (0, [1 s]), (1, [10 s]), (2, [100 s]),
)

#let panel(file, first: false) = cetz.canvas({
  import cetz.draw: *
  import cetz-plot: *

  let n = settings.len()
  let dx = 0.2 // x offset between neighbouring settings

  plot.plot(
    size: (4.2, 4),
    x-label: [$Q$],
    y-label: if first { [Execution time] } else { none },
    x-min: -0.5, x-max: 5.5,
    y-min: -3, y-max: 2,
    x-tick-step: none,
    x-ticks: qs.enumerate().map(((i, q)) => (i, str(q))),
    y-ticks: y-ticks,
    y-tick-step: none,
    x-grid: false, y-grid: true,
    legend: none,
    {
      for (i, (key, _, col)) in settings.enumerate() {
        let off = (i - (n - 1) / 2) * dx / 2
        let rows = series(file, key)
        if rows.len() == 0 { continue }
        let at(r) = r.x + off

        // Mean ± std: shaded band
        plot.add-fill-between(
          rows.map(r => (at(r), lg(r.m + r.s))),
          rows.map(r => (at(r), lg(r.m - r.s))),
          style: (stroke: none, fill: col.transparentize(75%)),
        )

        // Mean
        plot.add(
          rows.map(r => (at(r), lg(r.m))),
          mark: "o", mark-size: 0.12,
          style: (stroke: (paint: col, thickness: 1.2pt)),
          mark-style: (stroke: col, fill: white),
        )
      }
    },
  )
})

#let legend = {
  set text(size: 8pt)
  grid(
    columns: 3, column-gutter: 1.2em, row-gutter: 0.5em,
    ..settings.map(s => {
      box(width: 1em, height: 0.6em, fill: s.at(2).transparentize(80%), stroke: (top: 1.2pt + s.at(2), bottom: 1.2pt + s.at(2)))
      h(0.4em)
      s.at(1)
    }),
  )
}

#figure(
  {
    grid(
      columns: 3, column-gutter: 3.5em,
      ..files.enumerate().map(((i, f)) => align(center)[
        #text(size: 9pt, raw(f)) \
        #panel(f, first: i == 0)
      ]),
    )
    v(0.5em)
    legend
    v(0.3em)
    text(size: 8pt)[
      Line: mean; band: mean $plus.minus$ stddev. Log scale.
    ]
  },
  kind: "plot",
  supplement: [Figure],
  caption: [Execution time as a function of $Q$ for each benchmark ($k = 30$).],
)
