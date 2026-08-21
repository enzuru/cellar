# Cellar

**Warning: This app is AI generated and human reviewed. If that bothers you, move on.**

An Adwaita spreadsheet app for GNOME that uses Guile Scheme expressions instead of spreadsheet language. This is the ultimate marriage of the GNOME and GNU philosophies.

Every cell holds a **GNU Guile expression**. Double-click a cell and a real code
editor opens; whatever you write there is the cell. References like `A1` are
ordinary variables, so a cell can say `(+ A1 B1)` — or `(apply + (map (lambda (n)
(* n n)) (iota 10)))`, or anything else Guile can do.

Built with GTK4 and libadwaita, described in [Blueprint](https://gnome.pages.gitlab.gnome.org/blueprint-compiler/),
and driven entirely from Guile through [G-Golf](https://www.gnu.org/software/g-golf/).
**There is no C in this project.**

## Name

"Cellar" is a play on spreadsheet cells and Lisp cons cells, as this application uses both.

## Running it

```sh
nix develop      # Guile, G-Golf, GTK4, libadwaita, GtkSourceView, blueprint-compiler
make run
```

Open a sheet at startup with `make run FILE=example.cellar`, or build a
standalone wrapper with `nix build` and run `./result/bin/cellar`.

Run the model's test suite — it needs no display — with `make check`.

## Writing cells

|You type                      |You get                          |
|------------------------------|---------------------------------|
|`42`                          |`42`                             |
|`(* 6 7)`                     |`42`                             |
|`(+ A1 A2)`                   |the sum of two other cells       |
|`(string-upcase "hello")`     |`HELLO`                          |
|`(sum (range "A1" "A10"))`    |the sum of a rectangular range   |
|`(if (> A1 100) 'over 'under)`|a symbol                         |
|`(sort (list C1 C2 C3) <)`    |a list — all of Guile is in scope|

Bare references are bound automatically: any symbol in your code that looks like
a cell (`A1`, `AA30`) is bound to that cell's value before your expression runs.
Quoted data is untouched, so `'(A1 B1)` is still a list of two symbols.

Alongside all of `(guile)`, cells get a few helpers:

- `(cell "A1")` — a cell's value, when you need to compute the name
- `(range "A1" "B10")` — a flat list of values over a rectangle
- `(sum …)`, `(product …)`, `(average …)` / `(avg …)`, `(count …)`,
  `(cell-min …)`, `(cell-max …)` — these flatten their arguments and skip
  empty cells, so `(sum (range "A1" "A10"))` does the obvious thing

Errors stay local: a failing cell shows `#ERR` with the message in its tooltip,
and the rest of the sheet keeps working. Circular references are detected and
reported as the cycle they form, e.g. `A1 -> B1 -> A1`.

## Keyboard

|Key                                |Action                    |
|-----------------------------------|--------------------------|
|Arrows, Tab, Page Up/Down, Home/End|Move the active cell      |
|Double-click, or Enter             |Edit the active cell      |
|Ctrl+Return                        |Apply, while in the editor|
|Delete                             |Clear the active cell     |
|Ctrl+R                             |Recalculate               |
|Ctrl+O / Ctrl+S / Ctrl+Shift+S     |Open / Save / Save As     |
|Ctrl+Q                             |Quit                      |

## How the grid works

GTK4 ships no spreadsheet widget, and this project did not write one in C. The
grid is a `GtkColumnView`: one `GtkColumnViewColumn` per spreadsheet column,
each with a `GtkSignalListItemFactory` whose `setup` and `bind` callbacks close
over that column's index.

The interesting part is that the list model holds no data. It is a
`GtkStringList` of row numbers whose only job is to give the view the right row
count; at bind time each cell asks `gtk_column_view_cell_get_position()` for its
row, combines that with the column index from its closure, and looks the value
up in a Scheme hash table. That avoids the one thing that would have been
genuinely painful from a dynamic language — defining a custom `GObject` item
class for the model — and it means only the *visible* rows are ever realised.
A 100×26 sheet costs a few hundred widgets instead of 2,600, and it would scale
to 10,000 rows unchanged.

Double-click detection is a `GtkGestureClick` added to each cell's label in
`setup` (not `bind`, which would leak a controller on every scroll); the handler
filters on `n_press = 2`.

## Layout

```
flake.nix            dev shell + package; builds G-Golf from source
nix/g-golf.nix       G-Golf 0.8.7, patched for NixOS dlopen paths
ui/cellar.blp        main window: header bar, cell bar, column view
ui/editor.blp        the cell editor dialog (AdwDialog + GtkSourceView)
src/cellar/gi.scm    all GObject Introspection imports, in one place
src/cellar/model.scm the sheet: sources, evaluation, references — no GTK
src/cellar/grid.scm  the GtkColumnView spreadsheet
src/cellar/editor.scm the code editor and its live result preview
src/cellar/main.scm  application, actions, files
tests/model-test.scm headless tests for the model
```

`src/cellar/model.scm` deliberately has no GTK dependency, which is why the test
suite can run without a display.

## File format

A sheet is a readable Scheme alist of cell name to source text, so it diffs
well and you can edit it by hand:

```scheme
;; A Cellar sheet: an alist of cell name to Guile source.
(("A1" . "10") ("A2" . "32") ("A3" . "(+ A1 A2)"))
```

## Notes on G-Golf

Two things worth knowing if you extend this:

- G-Golf exposes every method under both a long name (`gtk-file-dialog-open`)
  and a short generic (`open`). Where the short name collides with core Guile —
  `open`, `close`, `ref`, `map` — use the long one, or you will get a confusing
  "no applicable method" from the wrong procedure.
- Handlers named in a `.blp`/`.ui` file are **not** supported: G-Golf has no
  GtkBuilder scope. Give the object an `id` and `connect` to it from Scheme.
- Async GIO methods take the `user_data` argument explicitly and their callback
  takes three arguments, so `gtk_file_dialog_save` is called as
  `(gtk-file-dialog-save dialog parent #f (lambda (dialog result data) …) #f)`.
  Closing over what you need is more reliable than the `data` argument.
- Keyboard handling on a `GtkColumnView` must use the **capture** phase
  (`(set-propagation-phase controller 'capture)`). The view binds the arrow keys
  for its own row navigation and will swallow them in the bubble phase.
- Packaging note: a wrapper must **set** `GI_TYPELIB_PATH`, not prepend to it.
  Inheriting a host path that points at a different glib makes GTK abort in
  `g_binding_class_init` at startup. Also note that glib's and pango's typelibs
  live in their `out` output, while `${glib}` refers to `bin`.

## What has been verified

The grid, the editor, evaluation, recalculation, error display, keyboard
navigation and the packaged `nix build` were all exercised end-to-end against a
real GTK build. Save/Open are the exception: each link was checked separately
(the async callback fires and returns a `GFile`, `get-path` yields a string, and
the save/load round-trip is covered by `make check`), but the four were never
driven together in one automated run, because `GtkFileDialog` hands off to the
desktop portal and its window cannot be scripted from a test harness.
