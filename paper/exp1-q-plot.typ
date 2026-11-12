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
  ("perf_simplifier-none_heuristic-on", "none, heuristic on", rgb("#d55e00")),
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

// Control: no simplifier, no heuristic (only measured at Q = 1, K = 30).
// Every Q is compared against this single value, so it is the flat line at 1×.
#let control = "perf_simplifier-none_heuristic-off"
#let control-mean(file) = {
  let rows = raw-rows.filter(r => r.File.ends-with("/" + file) and r.Settings == control and float(r.K) == k-fixed)
  float(rows.last().Mean)
}

#let l10(x) = calc.log(x, base: 10)
#let ymin = l10(0.25)
#let ymax = l10(4)
#let y-ticks = (
  (l10(0.25), [0.25×]), (l10(0.5), [0.5×]), (0, [1×]), (l10(2), [2×]), (l10(4), [4×]),
)

#let panel(file, first: false) = cetz.canvas({
  import cetz.draw: *
  import cetz-plot: *

  let n = settings.len()
  let dx = 0.2 // x offset between neighbouring settings
  let c = control-mean(file)

  plot.plot(
    size: (4.2, 4),
    x-label: [$Q$],
    y-label: if first { [Speedup over control] } else { none },
    x-min: -0.5, x-max: 5.5,
    y-min: ymin, y-max: ymax,
    x-tick-step: none,
    x-ticks: qs.enumerate().map(((i, q)) => (i, str(q))),
    y-ticks: y-ticks,
    y-tick-step: none,
    x-grid: false, y-grid: true,
    legend: none,
    {
      plot.annotate({
        cetz.draw.content((2.5, ymax + 0.15), anchor: "south", padding: -0.1, text(size: 9pt, raw(file)))
      })
      plot.add(((-0.5, 0), (5.5, 0)), style: (stroke: (paint: black, thickness: 0.8pt, dash: "dashed")))
      for (i, (key, _, col)) in settings.enumerate() {
        let off = (i - (n - 1) / 2) * dx / 2
        let rows = series(file, key)
        if rows.len() == 0 { continue }
        let at(r) = r.x + off

        // Mean ± std: shaded band
        plot.add-fill-between(
          rows.map(r => (at(r), l10(c / calc.max(r.m - r.s, 1e-9)))),
          rows.map(r => (at(r), l10(c / (r.m + r.s)))),
          style: (stroke: none, fill: col.transparentize(75%)),
        )

        // Mean
        plot.add(
          rows.map(r => (at(r), l10(c / r.m))),
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
        #panel(f, first: i == 0)
      ]),
    )
    v(0.5em)
    legend
    v(0.3em)
    text(size: 8pt)[
      Line: mean; band: mean $plus.minus$ stddev, of the setting time; speedup is control time (no simplifier, no heuristic, $Q = 1$) divided by setting time. Dashed line at 1×: no change; above it is faster than control. Log y axis.
    ]
  },
  kind: "plot",
  supplement: [Figure],
  caption: [Speedup over the control as a function of $Q$ for each benchmark ($k = 30$).],
)
