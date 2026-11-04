#import "@preview/cetz:0.4.2"
#import "@preview/cetz-plot:0.1.3"

// Execution time vs. K for benchExperiment1.
//
// Encoding per setting (one colour each):
//   line + marker   Mean
//   shaded band     Mean ± Stddev
//
// The K sweep is run at Q = 1 and, for two settings, also at Q = 16 (dashed
// line, square markers, same colour as the Q = 1 twin).
//
// The y axis is logarithmic (values are log10-transformed by hand, since
// cetz-plot has no log axes). Small x offsets keep the settings from overlapping.

#let raw-rows = csv("./benchExperiment1.csv", row-type: dictionary)

#let files = ("divByN.gcl", "memberOf.gcl", "pullUp.gcl")

// (key, label, colour, Q)
#let settings = (
  ("perf_simplifier-none_heuristic-off", "none, heuristic off", rgb("#e69f00"), 1),
  ("perf_simplifier-none_heuristic-on", "none, heuristic on", rgb("#d55e00"), 1),
  ("perf_simplifier-full_heuristic-off", "full, heuristic off", rgb("#56b4e9"), 1),
  ("perf_simplifier-full_heuristic-on", "full, heuristic on", rgb("#0072b2"), 1),
  ("perf_simplifier-full_heuristic-on-bypass", "full, heuristic on + bypass", rgb("#cc79a7"), 1),
  ("perf_simplifier-none_heuristic-on", "none, heuristic on", rgb("#d55e00"), 16),
  ("perf_simplifier-full_heuristic-on-bypass", "full, heuristic on + bypass", rgb("#cc79a7"), 16),
)

#let ks = (10, 20, 30, 40)
#let floor-val = 1e-3
#let lg(x) = calc.log(calc.max(x, floor-val), base: 10)

// Rows for one file/setting/Q as an array of dictionaries, sorted by K.
// Q and K are compared numerically (the CSV may say 1 or 1.0). The CSV also
// holds a Q sweep at K=30; later rows win on duplicates.
#let series(file, setting, q) = {
  let by-k = (:)
  for r in raw-rows {
    if r.File.ends-with("/" + file) and r.Settings == setting and float(r.Q) == q {
      by-k.insert(str(int(float(r.K))), r)
    }
  }
  ks.filter(k => str(k) in by-k).map(k => {
    let r = by-k.at(str(k))
    (
      k: k,
      m: float(r.Mean), mlb: float(r.MeanLB), mub: float(r.MeanUB),
      s: float(r.Stddev), slb: float(r.StddevLB), sub: float(r.StddevUB),
    )
  })
}

#let y-ticks = (
  (-3, [1 ms]), (-2, [10 ms]), (-1, [100 ms]), (0, [1 s]), (1, [10 s]), (2, [100 s]),
)

#let panel(file, first: false) = cetz.canvas({
  import cetz-plot: *

  let n = settings.len()
  let dx = 0.6 // x offset between neighbouring settings, in K units

  plot.plot(
    size: (4.2, 4),
    x-label: [$k$],
    y-label: if first { [Execution time] } else { none },
    x-min: 5, x-max: 45,
    y-min: -3, y-max: 2,
    x-tick-step: none,
    x-ticks: ks.map(k => (k, str(k))),
    y-ticks: y-ticks,
    y-tick-step: none,
    x-grid: false, y-grid: true,
    legend: none,
    {
      for (i, (key, _, col, q)) in settings.enumerate() {
        let dashed = q != 1
        let off = (i - (n - 1) / 2) * dx
        let rows = series(file, key, q)
        if rows.len() == 0 { continue }
        let at(r) = r.k + off

        // Mean ± std: shaded band
        plot.add-fill-between(
          rows.map(r => (at(r), lg(r.m + r.s))),
          rows.map(r => (at(r), lg(r.m - r.s))),
          style: (stroke: none, fill: col.transparentize(75%)),
        )

        // Mean
        plot.add(
          rows.map(r => (at(r), lg(r.m))),
          mark: if dashed { "square" } else { "o" }, mark-size: 0.12,
          style: (stroke: (paint: col, thickness: 1.2pt, dash: if dashed { "dashed" } else { "solid" })),
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
      let st = (paint: s.at(2), thickness: 1.2pt, dash: if s.at(3) != 1 { "dashed" } else { "solid" })
      box(width: 1.4em, height: 0.6em, fill: s.at(2).transparentize(80%), stroke: (top: st, bottom: st))
      h(0.4em)
      s.at(1)
      if s.at(3) != 1 [ ($Q = #s.at(3)$)]
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
      Solid: $Q = 1$; dashed: $Q = 16$.
    ]
  },
  kind: "plot",
  supplement: [Figure],
  caption: [Execution time as a function of $k$ for each benchmark.],
)
