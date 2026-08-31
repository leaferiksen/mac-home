---
name: emacs-config
description: Configure or debug this user's Emacs when editing init.el, elpa packages, or keybindings. Use whenever the task touches ~/.config/emacs, use-package, :bind/:hook, or a package under elpa/.
---

# Emacs config

## Where things actually live
- **Emacs binary**: installed via the homebrew formula
  `emacs-plus-app@next` (d12frosted/homebrew-emacs-plus tap). The app bundle
  lives at `/Users/leaf/Applications/Emacs.app`; on PATH it's just `emacs`
  (via `/opt/homebrew/bin/emacs` → the bundle's `Contents/MacOS/bin/emacs`).
  **Script with `emacs`, not the full path.**
- **Config root:** `~/.config/emacs/` (single `init.el`, plus `user-lisp/`)
- **Installed packages:** `~/.config/emacs/elpa/<name>-<version>/`
- **`use-package` & `bind-key` are built into the app bundle, NOT in an indexed
  workspace.** Read them via absolute path:
  `/Users/leaf/Applications/Emacs.app/Contents/Resources/lisp/use-package/*.el`
  and `.../lisp/bind-key.el`. Glob/Grep over the workspace will not
  find `use-package.el`.
  - **`lisp/bind-key.el`, NOT `lisp/emacs-lisp/bind-key.el`** — the latter
    path does not exist in this bundle.
- **Version:** before making any change, confirm the current version with
  `emacs --batch --eval '(princ emacs-version)'` — the user tracks Emacs
  closely, so behavior may differ from what this skill was written against.
  Check the version, and the specific code you're relying on, before you edit.

## Referencing bundle code
Emacs bundles change between patch releases, so never hardcode line numbers in
this skill or in code references. Cite by **function name** and how to find it:
- In Emacs: `C-h f use-package-normalize-binder RET`
- From shell (locate the file at runtime, then grep):
  ```bash
  emacs --batch --eval '(princ (locate-library "use-package-bind-key"))'
  # then
  grep -n "defun use-package-normalize-binder" <path-from-previous-line>
  ```
- Prefer `locate-library` over assuming a path — the binary resolves the file
  for you, so it stays correct after upgrades.

## `:bind` accepts only certain forms
The bundled `use-package-normalize-binder` (in `use-package-bind-key.el`, defalias'd as `use-package-normalize/:bind` and
`use-package-normalize/:bind*`) only accepts, inside `:bind` / `:bind*`, exactly:
- a `(KEY . CMD)` cons,
- one of the recognizer keys (see `defun` above for the authoritative list):
  `:map SYMBOL`, `:prefix STR`, `:prefix-map SYM`,
  `:prefix-docstring STR`, `:repeat-map SYM`, `:repeat-docstring STR`,
  `:filter SEXP`, `:menu-name STR`, `:package SYM`,
- or a nested list (recurses via `use-package-normalize-binder`).

A **bare mode-symbol** form like `:bind osx-dictionary ((map ...))` is rejected
with "wants arguments acceptable to the `bind-keys` macro".

To bind into a package's mode map, use the `:map` keyword:
```elisp
(use-package osx-dictionary
  :bind (:map osx-dictionary-mode-map
        ("C-u q" . my/osx-dictionary-kill)))
```

If the recognizer rejects your form, read the defun first:
`C-h f use-package-normalize-binder RET`, or locate-and-grep as shown above.
The bundled version may differ from the standalone `use-package` on Melpa.

## Fast test loop
Run in batch **before** editing `init.el`:
```bash
emacs --batch \
  -L ~/.config/emacs \
  -L ~/.config/emacs/elpa/<pkg-dir> \
  --eval '(<expr>)'
```
- Parse-check the whole file (signals on syntax error):
```elisp
(with-temp-buffer
  (insert-file-contents "~/.config/emacs/init.el")
  (read (current-buffer))
  t)
```
- Verify a binding actually landed:
```elisp
(require '<pkg> nil t)
(lookup-key <mode-map> (kbd "C-u q"))
```
- Load an elpa package: add `-L ~/.config/emacs/elpa/<name>-<version>/`.

## Conventions
- Function defs go in `:config`; bindings in `:bind` — keep them split.
- Detailed customization goes through `use-package` keywords (`:custom`,
  `:config`, `:hook`, `:bind`, `:defer`, `:after`), **not** through
  `custom-set-variables` / `custom-eval-after-init` blocks.
- **The user manages package installation themselves.** When a task touches
  packages, help with *customizing and debugging* installed ones — never
  suggest installing new packages, switching managers, or restructuring how
  packages are sourced.
- When a `use-package` keyword errors, **read the installed keyword's source**
  at the Resources path above before trying alternative forms.
