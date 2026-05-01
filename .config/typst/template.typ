#set page(
    paper: "us-letter",
    margin: 1in,
    header: context {align(right)[Eriksen #counter(page).display()]})
#set text(
    font: "Times New Roman",
    size: 12pt)
#set par(
    leading: 2em,
    spacing: 2em,
    justify: true,
    justification-limits: (tracking: (min: -0.01em, max: 0.02em)),
    linebreaks: "optimized")
#set table(
    inset: 6pt,
    stroke: none)

#show heading: it => {
    set text(
        size: 12pt,
        weight: "regular")
    set align(
        center)
    set block(
        above: 2em,
        below: 2em)
    it
}

Leaf Eriksen

$professor$

$class$

#datetime.today().display("[day] [month repr:long] [year]")

#set par(
    first-line-indent: (amount: 2em, all: true))

$body$

$if(works-cited)$
#context {
    let has_cites = query(cite).len() > 0
    let force_full = $if(full-bibliography)$true$else$false$endif$
    if has_cites or force_full {
        pagebreak(weak: true)
        align(center)[Works Cited]
    }
}

#set bibliography(style: "mla", title: none)
#set par(first-line-indent: 0in, hanging-indent: 0.5in) 
#bibliography("$works-cited$"$if(full-bibliography)$, full: true$endif$)
$endif$
