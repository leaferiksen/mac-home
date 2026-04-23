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

$author$ \
$professor$ \
$class$ \
#datetime.today().display("[day] [month repr:long] [year]")

#set par(
    first-line-indent: (amount: 2em, all: true))

$body$

#context {
    // Check if any citations actually exist in the document
    let has-citations = query(cite).len() > 0
    if has-citations {
        pagebreak(weak: true)
        align(center)[Works Cited]
        set par(first-line-indent: 0in, hanging-indent: 0.5in) 
        $if(bibliography)$
        set bibliography(style: "mla", title: none)
        bibliography(($for(bibliography)$"$bibliography$"$sep$,$endfor$))
        $endif$
    }
}
