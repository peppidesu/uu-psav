#import "@preview/cetz:0.4.2"
#import "@preview/cetz-plot:0.1.3"

// Metrics (stats.csv) vs. K and vs. Q, in the same style as exp1-plot.typ.
//
// Per setting (one colour each):
//   line + marker   mean over the runs (mtrMean*)
//   shaded band     mean ± std         (mtrStd*)
//   (stats.csv has no confidence intervals.)
//
// Layout: one figure = metrics as rows, benchmark files as columns, all
// settings in each panel. Every row shares one y range so panels compare.
//
// The y axis is log10(1 + x): counts can be 0, which a plain log axis cannot
// show. Tick labels show the original value.

#let raw-rows = csv("./stats.csv", row-type: dictionary)

#let files = ("divByN.gcl", "memberOf.gcl", "pullUp.gcl")

// (key, label, colour, Q) for the K sweep (Q = 16 variants are dashed).
#let k-settings = (
  ("perf_simplifier-none_heuristic-off", "none, heuristic off", rgb("#e69f00"), 1),
  ("perf_simplifier-none_heuristic-on", "none, heuristic on", rgb("#d55e00"), 1),
  ("perf_simplifier-full_heuristic-off", "full, heuristic off", rgb("#56b4e9"), 1),
  ("perf_simplifier-full_heuristic-on", "full, heuristic on", rgb("#0072b2"), 1),
  ("perf_simplifier-full_heuristic-on-bypass", "full, heuristic on + bypass", rgb("#cc79a7"), 1),
  ("perf_simplifier-none_heuristic-on", "none, heuristic on", rgb("#d55e00"), 16),
  ("perf_simplifier-full_heuristic-on-bypass", "full, heuristic on + bypass", rgb("#cc79a7"), 16),
)

// The Q sweep (K = 30, Q placed at equidistant positions).
#let q-settings = (
  ("perf_simplifier-none_heuristic-on", "none, heuristic on", rgb("#d55e00"), none),
  ("perf_simplifier-full_heuristic-on", "full, heuristic on", rgb("#0072b2"), none),
  ("perf_simplifier-full_heuristic-on-bypass", "full, heuristic on + bypass", rgb("#cc79a7"), none),
)

#let ks = (10, 20, 30, 40)
#let qs = (0, 1, 2, 4, 8, 16)
#let k-fixed = 30

#let modes = (
  k: (
    settings: k-settings,
    xs: ks.map(k => (k, str(k))),
    x-min: 5, x-max: 45,
    dx: 0.6,
    label: [$k$],
    xpos: r => if int(float(r.K)) in ks { int(float(r.K)) } else { none },
  ),
  q: (
    settings: q-settings,
    xs: qs.enumerate().map(((i, q)) => (i, str(q))),
    x-min: -0.5, x-max: 5.5,
    dx: 0.2,
    label: [$Q$],
    xpos: r => if float(r.K) == k-fixed { qs.position(q => q == float(r.Q)) } else { none },
  ),
)

// Metric groups: (column suffix, row label)
#let checks-metrics = (
  ("FeasibilityChecks", [Feasibility checks]),
  ("FeasibilityZ3", [Z3 checks]),
  ("FeasibilityTrivial", [Trivial checks]),
  ("FormulaLength", [Formula length]),
)
#let pruning-metrics = (
  ("FeasibilityPrunedZ3", [Paths pruned (Z3)]),
  ("FeasibilityPrunedTrivial", [Paths pruned (trivial)]),
  ("FeasibilityPrunedAmountZ3", [Pruned amount (Z3)]),
  ("FeasibilityPrunedAmountTrivial", [Pruned amount (trivial)]),
)

#let lg(x) = calc.log(calc.max(x, 0) + 1, base: 10)

// Rows for one file/setting as dictionaries (x, m, s) sorted by x. Later rows
// win on duplicates (the CSV holds the K = 30 point in both sweeps).
#let series(mode, file, setting, q, metric) = {
  let by-x = (:)
  // Trivial statistics are irrelevant when the simplifier is off: no series.
  if metric.contains("Trivial") and setting.contains("simplifier-none") { return () }
  for r in raw-rows {
    if r.fileName.ends-with("/" + file) and r.name == setting {
      if q != none and float(r.Q) != q { continue }
      let x = (modes.at(mode).xpos)(r)
      if x != none { by-x.insert(str(x), (x: x, m: float(r.at("mtrMean" + metric)), s: float(r.at("mtrStd" + metric)))) }
    }
  }
  by-x.values().sorted(key: p => p.x)
}

// Smallest power of ten (as an exponent) above every mean + std of the metric.
#let y-top(metric) = {
  let top = raw-rows.map(r => float(r.at("mtrMean" + metric)) + float(r.at("mtrStd" + metric))).fold(0, calc.max)
  calc.max(1, calc.ceil(lg(top)))
}

#let y-ticks-for(top) = range(0, top + 1).map(e => {
  let v = if e == 0 { 0 } else { calc.pow(10, e - 1) }
  (lg(v), if v == 0 [0] else if v == 1 [1] else [$10^#(e - 1)$])
})

#let panel(mode, file, metric, top, row-label: none, x-label: false) = cetz.canvas({
  import cetz-plot: *

  let m = modes.at(mode)
  let n = m.settings.len()

  plot.plot(
    size: (3.6, 2.6),
    x-label: if x-label { m.label } else { none },
    y-label: row-label,
    x-min: m.x-min, x-max: m.x-max,
    y-min: 0, y-max: top,
    x-tick-step: none,
    x-ticks: m.xs,
    y-ticks: y-ticks-for(top),
    y-tick-step: none,
    x-grid: false, y-grid: true,
    legend: none,
    {
      for (i, (key, _, col, q)) in m.settings.enumerate() {
        let dashed = q != none and q != 1
        let off = (i - (n - 1) / 2) * m.dx
        let rows = series(mode, file, key, q, metric)
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
          mark: if dashed { "square" } else { "o" }, mark-size: 0.1,
          style: (stroke: (paint: col, thickness: 1.2pt, dash: if dashed { "dashed" } else { "solid" })),
          mark-style: (stroke: col, fill: white),
        )
      }
    },
  )
})

#let legend(mode) = {
  set text(size: 8pt)
  grid(
    columns: 3, column-gutter: 1.2em, row-gutter: 0.5em,
    ..modes.at(mode).settings.map(s => {
      let dashed = s.at(3) != none and s.at(3) != 1
      let st = (paint: s.at(2), thickness: 1.2pt, dash: if dashed { "dashed" } else { "solid" })
      box(width: 1.4em, height: 0.6em, stroke: (top: st))
      h(0.4em)
      s.at(1)
      if dashed [ ($Q = #s.at(3)$)]
    }),
  )
}

#let metric-figure(mode, metrics, caption) = figure(
  {
    let tops = metrics.map(((key, _)) => y-top(key))
    grid(
      columns: 3, column-gutter: 3em, row-gutter: 0.8em,
      ..metrics.enumerate().map(((ri, (key, label))) => files.map(f => align(center)[
        #if ri == 0 { text(size: 9pt, raw(f)); linebreak() }
        #panel(mode, f, key, tops.at(ri),
          row-label: if f == files.first() { label } else { none },
          x-label: ri == metrics.len() - 1)
      ])).flatten(),
    )
    v(0.5em)
    legend(mode)
    v(0.3em)
    text(size: 8pt)[
      Line: mean over runs; band: mean $plus.minus$ std. Axis is $log_10(1 + x)$.
      #if mode == "k" [Solid: $Q = 1$; dashed: $Q = 16$.] else [$k = 30$.]
    ]
  },
  kind: "plot",
  supplement: [Figure],
  caption: caption,
)

#metric-figure("k", checks-metrics, [Feasibility checks and formula length as a function of $k$.])
#metric-figure("k", pruning-metrics, [Pruning as a function of $k$.])
#metric-figure("q", checks-metrics, [Feasibility checks and formula length as a function of $Q$ ($k = 30$).])
#metric-figure("q", pruning-metrics, [Pruning as a function of $Q$ ($k = 30$).])
