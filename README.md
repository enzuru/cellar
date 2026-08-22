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

With no sheet named it opens on a start screen: open a sheet, make one, or take
a scratch sheet that lives nowhere until you save it. Open one straight away
with `make run FILE=example.cellar`, or build a standalone wrapper with
`nix build` and run `./result/bin/cellar`.

Run the test suites — they need no display — with `make check`.

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
|`(sum (range 'A1 'A10))`      |the sum of a rectangular range   |
|`(if (> A1 100) 'over 'under)`|a symbol                         |
|`(sort (list C1 C2 C3) <)`    |a list — all of Guile is in scope|
|`(styled 42 #:background "red")`|`42`, on a red ground            |

Bare references are bound automatically: any symbol in your code that looks like
a cell (`A1`, `AA30`) is bound to that cell's value before your expression runs.
Quoted data is untouched, so `'(A1 B1)` is still a list of two symbols.

Where a helper needs the cell rather than its value, quote it: `'A1`. That is
the one way to write a reference — a string is only ever text — so a reference
is always recognisable, and moving a row rewrites every one of them. A computed
reference has to arrive as a symbol too, by way of `string->symbol`.

Alongside all of `(guile)`, cells get a few helpers:

- `(cell 'A1)` — a cell's value, when you need to compute which cell
- `(range 'A1 'B10)` — a flat list of values over a rectangle
- `(sum …)`, `(product …)`, `(average …)` / `(avg …)`, `(count …)`,
  `(cell-min …)`, `(cell-max …)` — these flatten their arguments and skip
  empty cells, so `(sum (range 'A1 'A10))` does the obvious thing
- `(styled value #:color … #:background …)` — the value, in colours of your
  choosing; see [Colour](#colour)

Errors stay local: a failing cell shows `#ERR` with the message in its tooltip,
and the rest of the sheet keeps working. Circular references are detected and
reported as the cycle they form, e.g. `A1 -> B1 -> A1`.

## Colour

A cell can say how it should be drawn:

```scheme
(styled (* B2 C2) #:color "#c01c28" #:background "#fff3b0")
```

Both keywords are optional, and a colour is either a hex literal (`#rgb` or
`#rrggbb`, with or without an alpha pair) or a colour name — `"red"`, or
`'red`. Anything else is an error in that cell, with the offending value in the
tooltip, rather than a stylesheet quietly dropping it on the floor.

The style is part of the value, which has two consequences worth knowing. The
first is that conditional formatting is an ordinary `if`:

```scheme
(if (> D8 500) (styled D8 #:color "#c01c28") D8)
```

The second is that colour keeps itself: it is written in the cell, so it
survives a save, a reload and a reordering exactly as the expression does,
with nothing on the side to keep in step. It also stays out of everyone else's
way — a cell that refers to a styled cell sees the plain value, so `(+ A1 B1)`
and `(sum (range 'A1 'A10))` do not care whether their operands are
coloured.

One thing to watch: a background on its own leaves the text in the theme's
colour, which under a dark theme is light. Pale fills want a `#:color` to go
with them.

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
are computed rather than written down — `(range 'A1 (corner-of my-table))` —
is the case this cannot see as a rectangle; each literal reference in it still
follows its own cell.

## Adding rows and columns

Right-click a row number in the gutter or a column header and take one of
*Insert Row Before*, *Insert Row After*, *Insert Column Before* or *Insert
Column After*. The right-click picks the line under the pointer before the menu
opens, so what you point at is what you act on, and it stays selected afterwards
so you can see what happened.

Ctrl+Alt with an arrow key does the same four things to the active cell, and so
does the main menu. The sheet grows by a line each time; it never runs out of
room at the bottom or the right the way a fixed grid would.

An insert shifts the cells below or right of it, and rewrites references exactly
as a move does — a sheet means after an insert what it meant before, and the row
that was `(* B2 C2)` still multiplies the same two cells once it has become
`(* B3 C3)`. The active cell stays on the cell it was on, so opening a row above
it carries it down.

Ranges are rectangles here too, and here the rectangle grows: a row opened
inside `(sum (range 'A1 'A3))` makes it `(sum (range 'A1 'A4))`, so whatever you
write in the new row is taken into the subtotal. A row opened above the range
pushes the whole range down instead, and one opened below it leaves it alone.

A sheet grows to fit what is read into it, so a file saved after an insert opens
at the size it was saved at rather than being trimmed back to the default 100×26.

## Keyboard

|Key                                |Action                    |
|-----------------------------------|--------------------------|
|Arrows, Tab, Page Up/Down, Home/End|Move the active cell      |
|Double-click, or Enter             |Edit the active cell      |
|Ctrl+Return                        |Apply, while in the editor|
|Delete                             |Clear the active cell     |
|Ctrl+Shift+Up/Down                 |Move the active row       |
|Ctrl+Shift+Left/Right              |Move the active column    |
|Ctrl+Alt+Up/Down                   |Insert a row before/after |
|Ctrl+Alt+Left/Right                |Insert a column before/after|
|Ctrl+R                             |Recalculate               |
|Ctrl+O / Ctrl+S / Ctrl+Shift+S     |Open / Save / Save As     |
|Ctrl+Q                             |Quit                      |

## How the grid works

GTK4 ships no spreadsheet widget, and this project did not write one in C. The
grid is a `GtkColumnView`: one `GtkColumnViewColumn` per spreadsheet column,
each with a `GtkSignalListItemFactory` whose `setup` and `bind` callbacks close
over a one-element box holding that column's index — a box rather than the
number itself, because inserting a column renumbers every column to its right
and there is nowhere to tell a callback that was installed once.

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

Colour goes through the stylesheet rather than through the widgets. Each
distinct (colour, background) pair a sheet asks for earns a generated CSS class
in a provider of the grid's own, added to the display above the application's;
a cell wears at most one of those classes at a time. The classes then outlive
the cell widgets `GtkColumnView` recycles underneath them, and the palette
stays as small as the sheet's actual use of colour.

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
src/cellar/store.scm sheets on disk: the folder format — no GTK
src/cellar/grid.scm  the GtkColumnView spreadsheet
src/cellar/editor.scm the code editor and its live result preview
src/cellar/main.scm  application, actions, the start screen
tests/model-test.scm headless tests for the model
tests/store-test.scm headless tests for the on-disk format
tests/gui-smoke.sh   drives the real UI under Xvfb (`make smoke`)
tests/gui-start-smoke.sh  the start screen, making a sheet, saving it
```

`src/cellar/model.scm` and `src/cellar/store.scm` deliberately have no GTK
dependency, which is why the test suites can run without a display.

## File format

A sheet is a folder, not a file. Every cell that holds anything is one small
file of Guile source under `cells/`, named for the cell, and a primary file at
the top holds what is true of the sheet rather than of any one cell.

```
budget.cellar/
  sheet.scm
  cells/
    A1.scm        "Qty"
    A2.scm        7
    D6.scm        (sum (range 'D2 'D4))
  .git/
```

```scheme
;; A Cellar sheet. The cells are in cells/, one file each.
((format . 1)
 (rows . 102)
 (columns . 28)
 (widths (1 . 181)))
```

The point of it is version control. A cell already holds source text, so giving
each one a file makes an edit to a cell a one-line diff, a cell's history
`git log -p cells/D6.scm`, and two people editing different corners of a sheet a
merge rather than a conflict. *New Sheet* offers to `git init` the folder for
you, ticked by default; nothing here commits on your behalf after that, and the
repository is yours to manage.

The cost is that a cell's name is its position, so inserting a row renames every
file below it and rewrites every reference to them. That is a loud diff, but an
honest one: the sheet really did change shape, and the references really did all
change with it.

Only files named exactly as a cell would be — `A1.scm`, `AA30.scm` — are read as
cells, and only those are ever deleted by a save. A `README.md` beside them, or a
`helpers.scm` in `cells/`, is yours and is left alone. A sheet can be opened by
its folder or by the primary file inside it.

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
still resizes the column instead of moving it. The same run colours a cell from
the editor, in a colour the palette has never seen, which is the case that
reloads the grid's CSS provider while the grid is on screen.
The folder format is covered twice over. `make check` writes real directories
in a temporary place and reads them back: the round trip, a cleared cell losing
its file, a moved row leaving none behind, and a `README.md` or a stray
`helpers.scm` in `cells/` surviving a save untouched. `tests/gui-start-smoke.sh`
then drives the same thing through the application — the start screen, *New
Sheet* with its git checkbox, writing a cell, saving — and reads the resulting
folder off disk rather than photographing it, so the assertions are on files
rather than on pixels.

Choosing a folder is the exception. Both *Open Sheet…* and the location button
in the New Sheet dialog hand off to the desktop portal, whose window cannot be
scripted from a test harness; each link there was checked separately (the async
callback fires and returns a `GFile`, and `get-path` yields a string). That is
why the New Sheet dialog fills its location in for you rather than making you
pick one, and why a sheet named on the command line opens without a dialog at
all: both paths stay testable.
