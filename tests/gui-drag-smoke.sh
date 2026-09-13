#!/usr/bin/env bash
#
# Drag a row by its number, and a column by its heading.
#
# GtkColumnView can reorder its own columns, but that would move the view's
# columns and not the sheet behind them, so Cellar turns that off and does the
# drag itself with a GtkGestureDrag.  This is the only test that exercises it:
# gui-smoke.sh has drag steps, but they come after the editor steps, and where
# the editor dialog does not take its keystrokes it stays open and swallows
# every click after it -- so a pass there proves nothing about dragging.
#
# Nothing here opens a dialog, for exactly that reason.
#
# What is checked is the folder, not the picture.  A drag that lands rewrites
# the cell files and the references inside them, so the disk says plainly
# whether the drag worked, and says it without anyone having to read a
# screenshot.
#
# Run it from inside `nix develop`:
#
#     nix develop -c nix shell nixpkgs#xvfb-run nixpkgs#imagemagick \
#       nixpkgs#xdotool nixpkgs#dbus -c xvfb-run -s "-screen 0 1280x820x24" \
#       tests/gui-drag-smoke.sh
#
# Screenshots land in ${OUT:-/tmp/cellar-drag-smoke}.

set -u
cd "$(dirname "$0")/.."

OUT="${OUT:-/tmp/cellar-drag-smoke}"
rm -rf "$OUT"
mkdir -p "$OUT"

export HOME="$OUT/home"
mkdir -p "$HOME"

export GDK_BACKEND=x11 GSK_RENDERER=cairo GUILE_AUTO_COMPILE=0

CELLAR="$(pwd)/.build/cellar"
if [ ! -x "$CELLAR" ]; then
  echo "build the shell first: make build"
  exit 1
fi
. tests/workbook.sh

failures=0
expect () {  # expect <description> <test...>
  local what="$1"; shift
  if "$@"; then echo "  ok   $what"; else echo "  FAIL $what"; failures=$((failures + 1)); fi
}
holds () { [ -f "$1" ] && [ "$(cat "$1")" = "$2" ]; }

settle () {  # settle <seconds> <test...>
  local limit="$1"; shift
  local waited=0
  while [ "$waited" -lt "$limit" ]; do
    if "$@"; then return 0; fi
    sleep 2
    waited=$((waited + 2))
  done
  "$@"
}

# Three rows that name each other, so that a move has to rewrite references as
# well as move files.  B1 says what A1 holds, wherever A1 ends up.
WORKBOOK="$OUT/drag.cellar"
SHEET="$WORKBOOK/sheets/Summary"
cellar_workbook "$WORKBOOK" Summary
cellar_cell "$SHEET" A1 '"first"'
cellar_cell "$SHEET" A2 '"second"'
cellar_cell "$SHEET" A3 '"third"'
cellar_cell "$SHEET" B1 'A1'

dbus-run-session -- "$CELLAR" "$WORKBOOK" > "$OUT/app.log" 2>&1 &
APP=$!
trap 'kill $APP 2>/dev/null; pkill -f "[.]build/cellar" 2>/dev/null' EXIT

sleep 15

shot () { import -window root "$OUT/$1.png"; echo "  captured $1.png"; }

# A gesture needs the pointer to actually travel: one jump from start to finish
# never crosses the drag threshold in a way GTK reports as a drag.
drag () {  # drag <from-x> <from-y> <to-x> <to-y>
  local fx="$1" fy="$2" tx="$3" ty="$4"
  xdotool mousemove "$fx" "$fy"; sleep 1
  xdotool mousedown 1; sleep 1
  local step
  for step in 1 2 3 4 5 6 7 8; do
    xdotool mousemove $(( fx + (tx - fx) * step / 8 )) $(( fy + (ty - fy) * step / 8 ))
    sleep 0.2
  done
  sleep 1
  xdotool mouseup 1
  sleep 2
}

# The grid: click a cell to give the window keyboard focus, since there is no
# window manager under Xvfb.
xdotool mousemove 220 180 click 1; sleep 2
shot 1-before

echo "1. dragging a row by its number"
# The row numbers are the narrow gutter down the left; rows are 25px apart with
# row 1 at y=180.  Row 1 down to row 3.
drag 35 180 35 230
shot 2-row-dropped

settle 30 holds "$SHEET/cells/A3.scm" '"first"'
expect "the dragged row landed where it was dropped" \
  holds "$SHEET/cells/A3.scm" '"first"'
expect "and the rows it passed slid up" holds "$SHEET/cells/A1.scm" '"second"'
expect "and the other one too" holds "$SHEET/cells/A2.scm" '"third"'
expect "the reference followed the cell it names" \
  holds "$SHEET/cells/B3.scm" 'A3'

echo "2. dragging a column by its heading"
# The headings sit at y=151; column A is around x=118 and column C around 325.
drag 118 151 325 151
shot 3-column-dropped

settle 30 holds "$SHEET/cells/C3.scm" '"first"'
expect "the dragged column landed where it was dropped" \
  holds "$SHEET/cells/C3.scm" '"first"'
expect "and its reference came with it" holds "$SHEET/cells/C1.scm" '"second"'

# And again.  A column that moves is taken out of the view and put back, and
# GTK builds a fresh heading when it does, so a drag that only works once is a
# drag whose gesture went with the old heading.
echo "3. dragging a column a second time"
drag 325 151 118 151
shot 4-column-dragged-back

settle 30 holds "$SHEET/cells/A3.scm" '"first"'
expect "a column can be dragged more than once" \
  holds "$SHEET/cells/A3.scm" '"first"'
expect "and its reference came back with it" holds "$SHEET/cells/A1.scm" '"second"'

# One column to the right, and one back to the left.  A column dropped on
# another takes its place and the ones between shift along, so a move of one is
# a swap -- and it is where an off-by-one would show if there were one.
echo "4. dragging a column one place and back"
drag 118 151 220 151
settle 30 holds "$SHEET/cells/B3.scm" '"first"'
expect "a column dropped on the next one takes its place" \
  holds "$SHEET/cells/B3.scm" '"first"'
expect "and took the rest of its own column with it" \
  holds "$SHEET/cells/B1.scm" '"second"'

drag 220 151 118 151
settle 30 holds "$SHEET/cells/A3.scm" '"first"'
expect "and the same drag the other way puts it back" \
  holds "$SHEET/cells/A3.scm" '"first"'

echo
echo "app log (excluding harmless environment noise):"
grep -av "libEGL\|DRI3\|dbus-daemon\|atk-bridge\|AT-SPI\|portal\|fusermount\|Registry\|display server" \
  "$OUT/app.log" | grep -av "^$" | head -10
echo "the sheet as it stands:"
for cell in "$SHEET"/cells/*.scm; do
  printf '  %s = %s\n' "$(basename "$cell" .scm)" "$(cat "$cell")"
done
echo "screenshots in $OUT"

if [ "$failures" -eq 0 ]; then
  echo "ALL TESTS PASSED"
else
  echo "$failures FAILURE(S)"
fi
exit "$failures"
