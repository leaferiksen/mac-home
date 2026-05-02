// --- Variables & Configuration ---
#let author = "Leaf Eriksen"
#let professor = "Professor $professor$"
#let class = "$class$"
#let works-cited = "$works-cited$"
#let has-word-count = $if(word-count)$true$else$false$endif$
#let words = counter("words")

// --- Helper Functions ---
#let count-words(it) = {
  show text: it => {
    words.update(n => n + it.text.split().len())
    it
  }
  it
}

// --- Document Settings ---
#set page(
  paper: "us-letter",
  margin: 1in,
  header: context { align(right)[#author #counter(page).display()] }
)

#set text(font: "Times New Roman", size: 12pt)

#set par(
  leading: 2em,
  spacing: 2em,
  justify: true,
  justification-limits: (tracking: (min: -0.01em, max: 0.02em)),
  linebreaks: "optimized"
)

#set table(inset: 6pt, stroke: none)

#show heading: it => {
  set text(size: 12pt, weight: "regular")
  set align(center)
  set block(above: 2em, below: 2em)
  it
}

// --- Metadata Section ---
#author \
#professor \
#class \
#datetime.today().display("[day] [month repr:long] [year]")
#if has-word-count [\ Word Count: #context words.final().at(0)]

// --- Body ---
#set par(first-line-indent: (amount: 2em, all: true))
#if has-word-count [
  #show: count-words
  $body$
] else [
  $body$
]

// --- Works Cited ---
$if(works-cited)$
#show bibliography: set par(first-line-indent: 0in, hanging-indent: 0.5in)
#set bibliography(style: "mla", title: none)

#context {
  let has_cites = query(cite).len() > 0
  let force_full = $if(full-bibliography)$true$else$false$endif$
  
  if has_cites or force_full {
    pagebreak(weak: true)
    align(center)[Works Cited]
    bibliography(works-cited $if(full-bibliography)$, full: true$endif$)
  }
}
$endif$
