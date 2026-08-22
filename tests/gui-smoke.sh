#!/usr/bin/env bash
#
# Drive the app under a nested X server and capture screenshots.
#
# Cellar has no display-free way to test the grid, so this clicks and types at
# it for real. Run it from inside `nix develop`:
#
#     nix develop -c nix shell nixpkgs#xvfb-run nixpkgs#imagemagick nixpkgs#xdotool \
#       nixpkgs#dbus -c xvfb-run -s "-screen 0 1280x820x24" tests/gui-smoke.sh
#
# Screenshots land in ${OUT:-/tmp/cellar-smoke}.
#
# WARNING: do not add a step that opens Save/Open. GtkFileDialog hands off to
# the XDG desktop portal over the session bus, so its window appears on your
# REAL desktop, not inside Xvfb -- and it cannot be scripted from here.

set -u
cd "$(dirname "$0")/.."

OUT="${OUT:-/tmp/cellar-smoke}"
mkdir -p "$OUT"

export GDK_BACKEND=x11 GSK_RENDERER=cairo GUILE_AUTO_COMPILE=0

# Preferences of its own. Step 10 turns on the external editor, and it has no
# business doing that to the Cellar you actually use.
export CELLAR_CONFIG="$OUT/config.scm"
rm -f "$CELLAR_CONFIG"
unset CELLAR_EDITOR

# dbus-run-session, because GApplication is single-instance: with a Cellar
# already running on your session bus this one would hand its activation to
# that window and exit, leaving nothing here to photograph.
# A copy of the example, because this script goes on to save over it.
SHEET="$OUT/example.cellar"
rm -rf "$SHEET"
cp -r example.cellar "$SHEET"

dbus-run-session -- guile -L src -s bin/cellar.scm "$SHEET" > "$OUT/app.log" 2>&1 &
APP=$!
trap 'kill $APP 2>/dev/null' EXIT

sleep 12

shot () { import -window root "$OUT/$1.png"; echo "  captured $1.png"; }

# The grid: click B2. Clicking is also what gives the window keyboard focus,
# since there is no window manager under Xvfb.
echo "1. grid"
xdotool mousemove 220 164 click 1; sleep 2
shot 1-grid

# Keyboard navigation: B2 -> D3.
echo "2. keyboard navigation"
xdotool key Right Right Down; sleep 2
shot 2-navigated

# Enter opens the editor on the active cell. The wait here and at every other
# editor-opening step is deliberately generous: building the dialog loads
# GtkSourceView's language and style scheme, and under Xvfb that has been seen
# to outlast a shorter one -- after which the keys below go to the grid behind
# the dialog and the step quietly does nothing.
echo "3. editor"
xdotool key Return; sleep 7
shot 3-editor

# Type a new expression and watch the live result, then apply with Ctrl+Return.
echo "4. edit and apply"
xdotool key ctrl+a; sleep 1
xdotool type --delay 30 '(if (string? A3) (string-length A3) (/ 1 0))'
sleep 3
shot 4-typed
xdotool key ctrl+Return; sleep 4
shot 5-applied

# An error cell should show #ERR without disturbing the rest of the sheet.
echo "5. error cell"
xdotool mousemove 530 139 click --repeat 2 --delay 60 1; sleep 7
xdotool key ctrl+a; sleep 1
xdotool type --delay 30 '(/ 1 0)'; sleep 2
xdotool key ctrl+Return; sleep 4
shot 6-error

# Reordering. Ctrl+Shift+arrow moves the active cell's whole row or column;
# every reference is rewritten with it, so the subtotal in the Total column has
# to come out of the move unchanged.
echo "6. reorder rows and columns"
xdotool mousemove 220 164 click 1; sleep 2
xdotool key ctrl+shift+Down; sleep 3
shot 7-row-moved
xdotool key ctrl+shift+Right; sleep 3
shot 8-column-moved
# And a move that would run off the edge of the sheet only says so.
xdotool key ctrl+shift+Left ctrl+shift+Left ctrl+shift+Left; sleep 3
shot 9-edge

# The same two moves with the mouse: rows are dragged by the number in the
# gutter, columns by their header. Watch the Total column across both -- the
# references move with the cells, so the subtotal must not change.
echo "7. drag a row and a column"
xdotool mousemove 35 189; sleep 1
xdotool mousedown 1; sleep 1
for y in 195 205 215 225 239; do xdotool mousemove 35 $y; sleep 0.3; done
sleep 1; shot 10-row-dragging
xdotool mouseup 1; sleep 2
shot 11-row-dropped

xdotool mousemove 220 111; sleep 1
xdotool mousedown 1; sleep 1
for x in 240 280 320 360 430; do xdotool mousemove $x 111; sleep 0.3; done
sleep 1; shot 12-column-dragging
xdotool mouseup 1; sleep 2
shot 13-column-dropped

# The edges of a header still resize the column rather than moving it.
xdotool mousemove 273 111; sleep 1
xdotool mousedown 1; sleep 1
for x in 300 330 350; do xdotool mousemove $x 111; sleep 0.3; done
xdotool mouseup 1; sleep 2
shot 14-resized

# A cell that colours itself. This is also the one step that exercises adding a
# colour the palette has never seen, which reloads the grid's CSS provider
# while the grid is on screen.
echo "8. a cell that colours itself"
xdotool mousemove 220 189 click --repeat 2 --delay 60 1; sleep 7
xdotool key ctrl+a; sleep 1
xdotool type --delay 30 '(styled 1234 #:color "#ffffff" #:background "#3584e4")'
sleep 2
xdotool key ctrl+Return; sleep 4
shot 15-coloured

# Adding rows and columns. Ctrl+Alt+arrow opens an empty line at the active
# cell; everything below or right of it shifts along, and the references shift
# with it, so the Total column has to survive this as it survives a move -- and
# a row opened inside the range it sums has to be taken into the range.
echo "9. add a row and a column"
xdotool mousemove 220 164 click 1; sleep 2
shot 16-before-insert
xdotool key ctrl+alt+Down; sleep 3
shot 17-row-added
xdotool key ctrl+alt+Right; sleep 3
shot 18-column-added
# The keyboard move still lands on the right column now that the letters have
# all shifted along.
xdotool key ctrl+shift+Left; sleep 3
shot 19-column-moved-after-insert

# And the new column's header drags like any other. Its header widget did not
# exist when the view was built, so a gesture that never reached it would show
# here as a drag that does nothing: watch the empty column C swap with Qty.
xdotool mousemove 400 111; sleep 1
xdotool mousedown 1; sleep 1
for x in 380 340 300 260; do xdotool mousemove $x 111; sleep 0.3; done
sleep 1; shot 20-new-column-dragging
xdotool mouseup 1; sleep 2
shot 21-new-column-dropped

# The context menu: right-clicking a row number or a column header picks that
# line and offers the four inserts. The row picked has to be the one under the
# pointer, not the one that was active before.
echo "10. the context menu on the gutter and on a header"
xdotool mousemove 35 314 click 3; sleep 3
shot 22-gutter-menu
# "Insert Row Before" is the first item, just under the pointer.
xdotool mousemove 120 334 click 1; sleep 3
shot 23-inserted-from-gutter
xdotool mousemove 400 111 click 3; sleep 3
shot 24-header-menu
xdotool key Escape; sleep 2

# The application icon, resolved by application id out of data/icons. The menu
# button is at 957 -- 1070 is the close button, which quits the app instead.
echo "11. about dialog"
xdotool mousemove 957 27 click 1; sleep 3
shot 25-menu
# Click "About Cellar" by position -- the last item, at the foot of the menu.
# Never press Return here: the highlighted item is "New Sheet...", and that
# opens a dialog instead. Check this offset against 25-menu.png whenever the
# menu gains an item, or the click lands on the wrong one: until this was
# fixed it had been landing on "Insert Column After", which quietly added a
# column to the sheet the assertions below then read.
xdotool mousemove 907 675 click 1; sleep 4
shot 26-about
xdotool key Escape; sleep 2

# What is on disk. Nothing is saved here: every step above wrote itself as it
# happened, so the folder already is the sheet. A sheet being a folder of one
# file per cell, that is something this script can read rather than photograph.
echo "12. the folder, which nobody saved"
shot 27-on-disk

failures=0
expect () {
  local what="$1"; shift
  if "$@"; then echo "  ok   $what"; else echo "  FAIL $what"; failures=$((failures + 1)); fi
}
contains () { grep -q "$2" "$1"; }

# The cell edited in step 4 has been carried around by every move since; it
# is at E2 by now, which is the point -- its file followed it.
expect "the edited cell is on disk under its new name" contains "$SHEET/cells/D2.scm" 'string-length'
expect "the subtotal came through the reordering" contains "$SHEET/cells/D7.scm" 'range'
expect "the sheet grew with the rows we added" contains "$SHEET/sheet.scm" 'rows . 10[0-9]'
expect "the column we widened was remembered" contains "$SHEET/sheet.scm" 'widths ('
expect "a cell nobody filled in has no file" test ! -f "$SHEET/cells/J20.scm"

# The external editor. A real one would sit there waiting for a human, so this
# stands in for it: a script that rewrites the file it is handed and exits, which
# is all Cellar asks of an editor.
echo "13. an external editor"
EDITOR_SCRIPT="$OUT/fake-editor.sh"
cat > "$EDITOR_SCRIPT" <<'SCRIPT'
#!/usr/bin/env bash
printf '(* 111 2)' > "$1"
SCRIPT
chmod +x "$EDITOR_SCRIPT"

# Turn it on in the preferences: the switch, then the command beneath it. The
# command row stays insensitive until the switch is on, so the order matters.
xdotool key ctrl+comma; sleep 4
xdotool mousemove 779 372 click 1; sleep 2
xdotool mousemove 500 427 click 1; sleep 1
xdotool type --delay 30 "$EDITOR_SCRIPT %s"
sleep 2
shot 28-preferences
xdotool key Escape; sleep 3

# Now Enter on a cell hands that script the cell's own file, and what the script
# writes there is the cell. Nothing is read back and nothing waits for the
# script to exit: the folder is watched, and that is how the edit arrives.
xdotool mousemove 220 164 click 1; sleep 2
xdotool key Return; sleep 8
shot 29-external-editor-applied
expect "the external editor wrote the cell's own file" \
  grep -rq '(\* 111 2)' "$SHEET/cells"

# That much only proves the script can write. The proof that Cellar *noticed*
# is to make Cellar write the whole sheet back out from what it holds in
# memory: inserting a row renames every cell file below it from the model. If
# the watcher had missed the edit, the stale model would overwrite it here and
# the expression would be gone.
xdotool key ctrl+alt+Down; sleep 4
shot 30-after-reload-and-insert
expect "and Cellar took it in, rather than overwriting it from a stale model" \
  grep -rq '(\* 111 2)' "$SHEET/cells"

echo
echo "app log (excluding harmless EGL noise):"
grep -av "libEGL\|DRI3" "$OUT/app.log" | head -5
echo "screenshots in $OUT"
if [ "$failures" -gt 0 ]; then echo "$failures FAILURE(S)"; exit 1; fi
echo "ALL CHECKS PASSED"
