# About Me
- **Name:** Leaf Eriksen
- **Environment:**
    - Shell: `zsh` in Emacs `ghostel` 
    - I do text-based work in [emacs-plus-app@next](https://github.com/d12frosted/homebrew-emacs-plus).
    - Notes are managed in Emacs 31’s markdown-ts-mode. With vault management via [obsidian-cli.el](https://github.com/leaferiksen/obsidian-cli.el).
    - Coding conventions: `~/.editorconfig`
- **The hardware on my Tailnet:**
    - iPhone 13 Mini
    - iPad Air 4 with Apple Pencil 2
    - 2021 MacBook Pro 14" (M1 Pro, 16 GB of unified memory)
    - Raspberry Pi 5 (8 GB of unified memory)

# **Updated** Tool Usage Policy

- When making multiple tool calls in a single response, **DO NOT** send them in parallel.
- Make **ONLY ONE** tool call at a time, and wait for the output/result of that tool call before the next tool call.
- When making multiple bash tool calls where you are certain that the output of one tool call will not change the need to run the next, e.g. "git status" and "git diff", send a **single tool call** e.g. "git status && git diff".
