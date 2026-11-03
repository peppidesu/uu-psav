#import "@preview/cetz:0.4.2"
#import "@preview/cetz-plot:0.1.3"

// Feasibility statistics (stats.csv) at a fixed K: one figure for a low K and one
// for a high K. Per benchmark file one panel with a stacked bar per setting; the
// bar height is the (mean) total number of checks, split into what happened to
// them. Settings are identified by the rotated label below each bar
// ("<simplifier>, <heuristic>").
//
// The counters in stats.csv overlap: the Z3 branch also runs the trivial
// (boolean literal) check, so a literal that Z3 refutes is counted as a Z3
// check, a trivial check, a Z3 prune and a trivial prune. To get disjoint
// categories (which sum to the total number of checks) a check is attributed to
// the trivial check whenever that check fired:
//
//   pruned (trivial)   = PrunedTrivial
//   pruned (Z3)        = PrunedZ3 - PrunedTrivial          (not pruned trivially)
//   feasible (Z3)      = (Z3 - Trivial) - pruned (Z3)      (Z3 calls on non-literals that passed)
//   skipped            = Checks - (the three above)        (heuristic skipped the check)
//
// ("Feasible (trivial)" never occurs in the data: every literal the trivial check
// saw was false. It is therefore not shown.)
//
// Trivial counters are ignored (set to 0) for settings with the simplifier off.
//
// This assumes every trivially checked formula also went through Z3, which holds
// for every configuration except `on-bypass`, where it is approximate (there are
// few trivial checks in that setting). Negative differences are clamped to 0.

#let raw-rows = csv("./stats.csv", row-type: dictionary)

#let files = ("divByN.gcl", "memberOf.gcl", "pullUp.gcl")
#let k-low = 10
#let k-high = 40

// (key, label, colour, Q). The colour is unused here (kept for consistency with
// the other plots); bars are identified by their label.
#let settings = (
  ("perf_simplifier-none_heuristic-off", "none, heuristic off", rgb("#e69f00"), 1),
  ("perf_simplifier-none_heuristic-on-bypass", "none, heuristic on", rgb("#d55e00"), 1),
  ("perf_simplifier-full_heuristic-off", "full, heuristic off", rgb("#56b4e9"), 1),
  ("perf_simplifier-full_heuristic-on", "full, heuristic on", rgb("#0072b2"), 1),
  ("perf_simplifier-full_heuristic-on-bypass", "full, heuristic on + bypass", rgb("#cc79a7"), 1),
  ("perf_simplifier-none_heuristic-on-bypass", "none, heuristic on", rgb("#d55e00"), 16),
  ("perf_simplifier-full_heuristic-on", "full, heuristic on", rgb("#0072b2"), 16),
)

// Stack order, bottom to top: (label, colour)
// Greens: pruned (dark = trivial, light = Z3); red: feasible (Z3), i.e. a
// check that did not prune; grey: skipped. Deliberately unrelated to the setting
// colours.
#let categories = (
  ("Pruned (trivial)", rgb("#1b5e3a")),
  ("Pruned (Z3)", rgb("#52b788")),
  ("Feasible (Z3)", rgb("#d1495b")),
  ("Skipped by heuristic", rgb("#e0e0e0")),
)

#let metric(r, key) = float(r.at("mtrMean" + key))

// Disjoint category values for one CSV row, in `categories` order.
#let split(r) = {
  let c = metric(r, "FeasibilityChecks")
  let z = metric(r, "FeasibilityZ3")
  // With the simplifier off the trivial check is irrelevant: ignore its counters.
  let simplifier-off = r.name.contains("simplifier-none")
  let t = if simplifier-off { 0 } else { metric(r, "FeasibilityTrivial") }
  let pz = metric(r, "FeasibilityPrunedZ3")
  let pt = if simplifier-off { 0 } else { metric(r, "FeasibilityPrunedTrivial") }
  let pruned-t = pt
  let pruned-z = calc.max(pz - pt, 0)
  let ok-z = calc.max(z - calc.min(t, z) - pruned-z, 0)
  let skipped = calc.max(c - (pruned-t + pruned-z + ok-z), 0)
  (pruned-t, pruned-z, ok-z, skipped)
}

// Category values for one file/setting/Q at the given K, or none if the CSV has
// no such run. Later rows win on duplicates (the CSV holds some points twice).
#let bar(file, setting, q, k) = {
  let found = none
  for r in raw-rows {
    if r.fileName.ends-with("/" + file) and r.name == setting and float(r.Q) == q and float(r.K) == k {
      found = split(r)
    }
  }
  found
}

// Round `top` up to a tick step of 1, 2 or 5 times a power of ten (about 4 ticks).
#let nice-step(top) = {
  let raw = calc.max(top, 1) / 4
  let mag = calc.pow(10, calc.floor(calc.log(raw, base: 10)))
  let m = (1, 2, 5, 10).find(m => m * mag >= raw)
  m * mag
}

// A panel whose largest bar dwarfs all others (more than `break-ratio` times the
// second largest) gets a broken y axis: the lower part shows 0 .. just above the
// second largest bar, the upper part only the top of the outlier, with a tear in
// between. The axis is drawn in normalised units; `t` maps values onto it.
#let break-ratio = 6
#let lower-end = 0.7 // normalised extent of the lower part
#let upper-start = 0.8 // normalised start of the upper part

#let panel(file, k, first: false) = cetz.canvas({
  import cetz-plot: *
  cetz.draw.set-style(axes: (grid-layer: -1, grid: (stroke: 0.4pt + luma(200))))

  let bars = settings.map(((key, _, col, q)) => bar(file, key, q, k))
  let totals = bars.filter(b => b != none).map(b => b.sum()).sorted()
  let top = totals.last()
  let second = if totals.len() > 1 { totals.at(totals.len() - 2) } else { 0 }
  let broken = second > 0 and top > break-ratio * second

  let step = nice-step(top)
  let ymax = calc.ceil(top / step) * step

  // lower/upper ranges of a broken axis, in data units
  let s1 = nice-step(second)
  let lo = calc.ceil(second * 1.05 / s1) * s1
  let s2 = nice-step(top * 0.25)
  let hi = calc.floor(top * 0.8 / s2) * s2
  let up = calc.ceil(top / s2) * s2

  let t(y) = if not broken { y } else if y <= lo {
    lower-end * y / lo
  } else if y >= hi {
    upper-start + (1 - upper-start) * (y - hi) / (up - hi)
  } else {
    lower-end + (upper-start - lower-end) * (y - lo) / (hi - lo)
  }

  let y-ticks = if broken {
    let lower = range(0, int(calc.round(lo / s1)) + 1).map(i => (t(i * s1), [#(i * s1)]))
    let upper = (hi, up).map(v => (t(v), [#v])) // only two: the upper part is short
    lower + upper
  } else {
    range(0, int(calc.round(ymax / step)) + 1).map(i => (i * step, [#(i * step)]))
  }

  let x-lo = -0.6
  let x-hi = settings.len() - 0.4
  let short(label, q) = text(size: 7pt, rotate(-90deg, reflow: true, label.replace("heuristic ", "") + if q != 1 [ ($Q = #q$)]))

  plot.plot(
    size: (4.2, 3.2),
    x-label: none,
    y-label: if first { [Checks ($k = #k$)] } else { none },
    x-min: x-lo, x-max: x-hi,
    y-min: 0, y-max: if broken { 1 } else { ymax },
    x-tick-step: none,
    x-ticks: settings.enumerate().map(((i, (_, label, _, q))) => (i, short(label, q))),
    y-ticks: y-ticks,
    y-tick-step: none,
    x-grid: false, y-grid: true,
    legend: none,
    {
      for (i, (key, _, col, q)) in settings.enumerate() {
        let parts = bars.at(i)
        if parts == none { continue }
        let x0 = i - 0.4
        let x1 = i + 0.4
        let y = 0
        for (ci, v) in parts.enumerate() {
          if v > 0 {
            plot.add-fill-between(
              ((x0, t(y + v)), (x1, t(y + v))),
              ((x0, t(y)), (x1, t(y))),
              style: (stroke: 0.3pt + white, fill: categories.at(ci).at(1)),
            )
          }
          y += v
        }
      }

      if broken {
        // the tear: blank out the gap, then slanted strokes on both axes
        plot.add-fill-between(
          ((x-lo, upper-start - 0.01), (x-hi, upper-start - 0.01)),
          ((x-lo, lower-end + 0.01), (x-hi, lower-end + 0.01)),
          style: (stroke: none, fill: white),
        )
        for (xa, xb) in ((x-lo, x-lo + 0.35), (x-hi - 0.35, x-hi)) {
          for (ya, yb) in ((lower-end - 0.01, lower-end + 0.03), (upper-start - 0.03, upper-start + 0.01)) {
            plot.add(((xa, ya), (xb, yb)), style: (stroke: 0.8pt + black))
          }
        }
      }
    },
  )
})

#let legend = {
  set text(size: 8pt)
  grid(
    columns: 4, column-gutter: 1.2em,
    ..categories.map(((label, col)) => {
      box(width: 0.8em, height: 0.8em, fill: col, stroke: 0.4pt + luma(120))
      h(0.4em)
      label
    }),
  )
}

#figure(
  {
    grid(
      columns: 3, column-gutter: 2.5em, row-gutter: 0.8em,
      ..(k-low, k-high).map(k => files.enumerate().map(((i, f)) => align(center)[
        #if k == k-low { text(size: 9pt, raw(f)); linebreak() }
        #panel(f, k, first: i == 0)
      ])).flatten(),
    )
    v(0.5em)
    legend
    v(0.3em)
    text(size: 8pt)[
      Bar height: mean total number of checks, split by outcome. Labels give the
      simplifier and heuristic setting; all use $Q = 1$ unless marked.
    ]
  },
  kind: "plot",
  supplement: [Figure],
  caption: [Feasibility checks per benchmark at low $k$ (top) and high $k$ (bottom).],
)
