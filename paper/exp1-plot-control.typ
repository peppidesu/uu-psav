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

// Control: no simplifier, no heuristic, Q = 1. Bars show speedup over it.
#let control = "perf_simplifier-none_heuristic-off"

// (key, label, colour, Q)
#let settings = (
  // ("perf_simplifier-none_heuristic-off", "none, heuristic off", rgb("#e69f00"), 1),
  ("perf_simplifier-none_heuristic-on", "none, heuristic on", rgb("#d52e00"), 1),
  ("perf_simplifier-full_heuristic-off", "full, heuristic off", rgb("#3684b9"), 1),
  ("perf_simplifier-full_heuristic-on", "full, heuristic on", rgb("#f2a252"), 1),
  ("perf_simplifier-full_heuristic-on-bypass", "full, heuristic on + bypass", rgb("#5ea530"), 1),
  //("perf_simplifier-none_heuristic-on", "none, heuristic on", rgb("#d55e00"), 16),
  //("perf_simplifier-full_heuristic-on-bypass", "full, heuristic on + bypass", rgb("#cc79a7"), 16),
)

#let ks = (10, 20, 30, 40, 50, 60)
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

#let ticks = {
  (
    (-3, [1 ms]), (-2, [10 ms]), (-1, [100 ms]), (0, [1 s]), (1, [10 s]),
  )
}

// x = k (linear), y = setting time / control time (log), one point per k
// (connected in k order). The dashed horizontal line at 1x is the control itself:
// below it is faster than control, above it is slower.
#let xticks = ((10, [10]), (20, [20]), (30, [30]), (40, [40]))
#let yticks = (
  (calc.log(0.25, base: 10), [0.25×]), (calc.log(0.5, base: 10), [0.5×]),
  (0, [1×]),
  (calc.log(2, base: 10), [2×]), (calc.log(4, base: 10), [4×]),
)

// Control time in ms below 1 s, otherwise in s.
#let fmt-time(sec) = if sec < 1 { [#calc.round(sec * 1000, digits: if sec < 0.1 { 1 } else { 0 }) ms] } else { [#calc.round(sec, digits: 2) s] }

#let panel(file, first: false) = cetz.canvas({
  import cetz-plot: *

  let ctrl = series(file, control, 1)

  plot.plot(
    name: "p",
    size: (4, 4),
    x-label: [$k$],
    y-label: if first { [Time (relative to control)] } else { none },
    x-min: 4, x-max: 46, y-min: calc.log(0.25, base: 10), y-max: calc.log(4, base: 10),
    x-tick-step: none, x-ticks: xticks,
    y-tick-step: none, y-ticks: yticks,
    x-grid: true, y-grid: true,
    legend: none,
    {
      plot.annotate({
        cetz.draw.content((25, calc.log(4, base: 10) + 0.3), anchor: "south", padding: -0.05, text(size: 10pt, raw(file)))
      })
      for c in ctrl {
        plot.annotate({
          cetz.draw.content((c.k, calc.log(4, base: 10)), anchor: "north", padding: 0, text(size: 6pt, fmt-time(c.m)))
        })
      }
      plot.add(((4, 0), (46, 0)), style: (stroke: (paint: black, thickness: 0.8pt, dash: "dashed")))
      for (key, _, col, q) in settings {
        let pts = series(file, key, q).map(r => {
          let c = ctrl.find(c => c.k == r.k)
          if c == none { none } else { (r.k, calc.log(r.m / c.m, base: 10)) }
        }).filter(p => p != none)
        if pts.len() == 0 { continue }
        let rows = series(file, key, q).filter(r => ctrl.find(c => c.k == r.k) != none)
        let band(sign) = rows.map(r => {
          let c = ctrl.find(c => c.k == r.k)
          (r.k, calc.log(calc.max(r.m + sign * r.s, 1e-9) / c.m, base: 10))
        })
        plot.add-fill-between(
          band(1), band(-1),
          style: (stroke: none, fill: col.transparentize(80%)),
        )
        plot.add(
          pts,
          mark: "o", mark-size: 0.07,
          style: (stroke: (paint: col, thickness: 0.8pt)),
          mark-style: (stroke: col, fill: col),
        )
      }
    },
  )
})

#let legend = {
  set text(size: 8pt)
  grid(
    columns: 2, column-gutter: 1.2em, row-gutter: 0.5em,
    ..settings.map(s => {
      box(width: 1.4em, height: 0.6em, stroke: (top: (paint: s.at(2), thickness: 1.2pt)))
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
        #panel(f, first: i == 0)
      ]),
    )
    legend
    text(size: 8pt)[

    ]
  },
  kind: "plot",
  supplement: [Figure],
  caption: [Execution time of different configurations relative to control for increasing $k$],
)
