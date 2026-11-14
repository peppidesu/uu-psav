#import "@preview/cetz:0.4.2"
#import "@preview/cetz-plot:0.1.3"

#let raw-rows = csv("./killed.csv", row-type: dictionary)

#let panel = cetz.canvas({
  import cetz-plot: *

  // let dx = 0.6 // x offset between neighbouring settings, in K units

  plot.plot(
    size: (3, 4),
    x-label: [$N$],
    y-label: [Mutants killed],
    x-min: 2, x-max: 10,
    x-tick-step: 1,
    y-min: 21, y-max: 33,
    y-tick-step: 1,
    x-grid: false, y-grid: true,
    legend: none,
    {
        let rows = raw-rows.map(x=> (float(x.N), float(x.count)))

        // Mean
        plot.add(
          rows,
          // mark: "square", mark-size: 0.12,
          // style: (stroke: (paint: "green", thickness: 1.2pt, dash:  "solid")),
          // mark-style: (stroke: "green", fill: white),
        )
    },
  )
})


// #let legend = {
//   set text(size: 8pt)
//   grid(
//     columns: 3, column-gutter: 1.2em, row-gutter: 0.5em,
//     ..settings.map(s => {
//       let st = (paint: s.at(2), thickness: 1.2pt, dash: if s.at(3) != 1 { "dashed" } else { "solid" })
//       box(width: 1.4em, height: 0.6em, fill: s.at(2).transparentize(80%), stroke: (top: st, bottom: st))
//       h(0.4em)
//       s.at(1)
//       if s.at(3) != 1 [ ($Q = #s.at(3)$)]
//     }),
//   )
// }

#figure(
  {
    panel
  },
  kind: "plot",
  supplement: [Figure],
  caption: [ Number of mutants killed out of the 47 for increasing N],
)
