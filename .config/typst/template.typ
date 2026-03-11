#set text(
    font: "Times New Roman",
    size: 12pt
)

#set par(
    leading: 1em,
    justify: true,
    justification-limits: (tracking: (min: -0.01em, max: 0.02em)),
    linebreaks: "optimized"
)

#set page(
    margin: (x: 1in, y: 1in)
)

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#show heading: set block(below: 1em)

#show terms.item: it => block(breakable: false)[
    #text(weight: "bold")[#it.term]
    #block(inset: (left: 1.5em, top: -0.4em))[#it.description]
]

#set table(
    inset: 6pt,
    stroke: none
)

$body$

