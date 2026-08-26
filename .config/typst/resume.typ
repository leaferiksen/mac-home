#set page(
    paper: "us-letter",
    margin: .75in,
)

#set text(font: ("Atkinson Hyperlegible Next", "Symbols Nerd Font"), size: 11pt)

#show heading.where(level: 1): set text(size: 24pt)

#set par(spacing: 1em)

#set list(marker: ([], [‣], [–]))

#show quote: set text(fill: luma(80))

$body$
