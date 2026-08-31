---
name: emacs-config
description: Configure or debug this user's Emacs when editing init.el, elpa packages, or keybindings. Use whenever the task touches ~/.config/emacs, use-package, :bind/:hook, or a package under elpa/.
---

# Emacs config

## Where things actually live
- **Emacs binary** (homebrew symlink, NOT a brew Cellar install):
  `/Users/leaf/Applications/Emacs.app/Contents/MacOS/bin/emacs`
  (also reachable via `/opt/homebrew/bin/emacs`)
- **Config root:** `~/.config/emacs/` (single `init.el`, plus `user-lisp/`)
- **Installed packages:** `~/.config/emacs/elpa/<name>-<version>/`
- **`use-package` & `bind-keys` are built into the app bundle, NOT in an indexed
  workspace.** Read them via absolute path:
  `/Users/leaf/Applications/Emacs.app/Contents/Resources/lisp/use-package/*.el`
  and `.../lisp/emacs-lisp/bind-key.el`. Glob/Grep over the workspace will not
  find `use-package.el`.

## Hard constraint (caused a bind-keys parse error)
This Emacs (31.x) built-in `use-package-normalize-binder` only accepts, inside
`:bind` / `:bind*`, exactly:
- a `(KEY . CMD)` cons,
- one of the recognizer keys from `use-package-normalize-binder`
  (use-package-bind-key.el:92): `:map SYMBOL`, `:prefix STR`,
  `:prefix-map SYM`, `:prefix-docstring STR`, `:repeat-map SYM`,
  `:repeat-docstring STR`, `:filter SEXP`, `:menu-name STR`,
- or a nested list.

A **bare mode-symbol** form like `:bind osx-dictionary ((map ...))` is rejected
with "wants arguments acceptable to the `bind-keys` macro".

To bind into a package's mode map, use the `:map` keyword:
```elisp
(use-package osx-dictionary
  :bind (:map osx-dictionary-mode-map
        ("C-u q" . my/osx-dictionary-kill)))
```

## Fast test loop (run in batch before touching init.el)
```bash
/Applications/Emacs.app/Contents/MacOS/bin/emacs --batch \
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
- When a `use-package` keyword errors, **read the installed keyword's source**
  at the Resources path above before trying alternative forms. The bundled
  version may differ from the standalone package on Melpa.
