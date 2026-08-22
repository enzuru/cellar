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

Both `make run` and `./result/bin/cellar` give you a window carrying the
desktop's fallback icon. That is expected: the icon appears only once Cellar is
*installed* somewhere the desktop already looks — `nix profile install .`, for
instance. See the note under [Notes on G-Golf](#notes-on-g-golf).

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

## Reordering rows and columns

Drag a row by the number in the gutter, or a column by its header, and drop it
where you want it: the line you are holding dims, the one it would land on
lights up. The edges of a header still resize the column, as they always did.

Ctrl+Shift with an arrow key does the same thing one place at a time, and the
same four moves are in the main menu. Either way the active cell stays on the
cell it was on, so you can hold the shortcut down and walk a row to where you
want it.

A move rewrites the sheet, not just the screen. Every reference in every cell is
put through the same permutation as the cells themselves, so `(* B2 C2)` becomes
`(* B3 C3)` when its row slides down and a sheet means exactly what it meant
before it was rearranged. References are rewritten in the source text, so your
formatting, line breaks and comments come back untouched.

Ranges are handled as rectangles rather than as their two corners: when both
ends of a move are inside a range, the range keeps its extent and only the
contents shuffle — reordering the lines of a table does not change its
subtotal. When a row is moved out of a range it drops out of it, and a row moved
in is picked up, which is what a spreadsheet should do. A range whose corners
are computed rather than written down — `(range "A1" (corner-of my-table))` —
is the case this cannot see as a rectangle; each literal reference in it still
follows its own cell.

## Keyboard

|Key                                |Action                    |
|-----------------------------------|--------------------------|
|Arrows, Tab, Page Up/Down, Home/End|Move the active cell      |
|Double-click, or Enter             |Edit the active cell      |
|Ctrl+Return                        |Apply, while in the editor|
|Delete                             |Clear the active cell     |
|Ctrl+Shift+Up/Down                 |Move the active row       |
|Ctrl+Shift+Left/Right              |Move the active column    |
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

Dragging works the same way round. `GtkColumnView` can reorder its own columns,
but that moves the view's columns and not the sheet behind them — the letters
would come out in the wrong order and A1 would no longer be the cell in the
corner — so the view's reordering stays off and the drag is a `GtkGestureDrag`
on the gutter cell and on the column header, ending in the same `move-row!` and
`move-column!` the keyboard uses. The row under the pointer is whatever
`gtk_widget_pick` finds there, matched against the cells the factories handed
us; the column is found by measuring the header widgets.

## Layout

```
flake.nix            dev shell + package; builds G-Golf from source
nix/g-golf.nix       G-Golf 0.8.7, patched for NixOS dlopen paths
ui/cellar.blp        main window: header bar, cell bar, column view
ui/editor.blp        the cell editor dialog (AdwDialog + GtkSourceView)
data/dev.enzuru.Cellar.desktop  the desktop entry; the file name is the application id
data/icons/hicolor/  the application icon, full colour and symbolic
src/cellar/gi.scm    all GObject Introspection imports, in one place
src/cellar/model.scm the sheet: sources, evaluation, references — no GTK
src/cellar/grid.scm  the GtkColumnView spreadsheet
src/cellar/editor.scm the code editor and its live result preview
src/cellar/main.scm  application, actions, files
tests/model-test.scm headless tests for the model
tests/gui-smoke.sh   drives the real UI under Xvfb (`make smoke`)
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
- The window icon is not the application's to set, and there is no way to make
  an uninstalled build show one. `gtk_window_set_icon_name` is an X11-only call
  that Wayland ignores; there the compositor matches the toplevel's application
  id against an installed `<id>.desktop` and reads its `Icon=` key. Since that
  lookup happens in the compositor's process, nothing the app does reaches it:
  `install-icons`' search path only feeds the app's own `GtkIconTheme` (which is
  why the about dialog is fine), and the wrapper's `XDG_DATA_DIRS` only covers
  the app's process, not GNOME Shell's. GTK 4.22 does implement
  `xdg-toplevel-icon-v1`, which would let a client hand the compositor rendered
  pixels instead, but mutter 50 does not implement the other half, so the global
  is never advertised. Installing the desktop entry into a prefix the session
  already searches is the only thing that works.
- GTK's drag-and-drop is out of reach from Guile: `gdk_content_provider_new_for_value`
  takes a `GValue` and `gtk_drop_target_new` takes a `GType`, and G-Golf marshals
  neither. Reordering therefore drags with a `GtkGestureDrag`, which suits it
  better anyway — nothing is being transferred, and what is dragged never
  leaves the process.
- A gesture on a column header has to be in the **capture** phase. The header
  is a `GtkColumnViewTitle` with gestures of GTK's own, and one of them claims
  the sequence as soon as the pointer moves: a bubble-phase gesture there sees
  the button press and then nothing else, which looks exactly like a drag that
  silently does not work.
- The header widgets are not the column's to hand out — a `GtkColumnViewColumn`
  has a title string, not a header factory — so they are reached by walking the
  view: its first child is the header row, whose children are the titles in
  column order. They exist as soon as the columns are appended, before the view
  is realised.
- GObject identity survives the round trip: the same GObject always comes back
  as the same GOOPS instance, so `eq?` is a reliable way to recognise a widget
  handed back by `gtk_widget_pick`. That is what lets a drag find the cell under
  the pointer without any coordinate arithmetic.
- Packaging note: a wrapper must **set** `GI_TYPELIB_PATH`, not prepend to it.
  Inheriting a host path that points at a different glib makes GTK abort in
  `g_binding_class_init` at startup. Also note that glib's and pango's typelibs
  live in their `out` output, while `${glib}` refers to `bin`.

## What has been verified

The grid, the editor, evaluation, recalculation, error display, keyboard
navigation, row and column reordering, and the packaged `nix build` were all
exercised end-to-end against a real GTK build, most of it by `make smoke`, which
also confirms that the application icon resolves by application id and renders
in the about dialog. Reordering was driven the same way, against
`example.cellar`, by keyboard and by mouse: rows and columns move, the active
cell follows, the subtotal and tax hold their values across the move, a move at
the edge of the sheet says so in a toast, and dragging the edge of a header
still resizes the column instead of moving it.
Save/Open are the exception: each link was checked separately (the async
callback fires and returns a `GFile`, `get-path` yields a string, and the
save/load round-trip is covered by `make check`), but the four were never driven
together in one automated run, because `GtkFileDialog` hands off to the desktop
portal and its window cannot be scripted from a test harness.
