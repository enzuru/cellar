#!/usr/bin/env bash
#
# The menu on a row number and on a column heading, and the delete shortcuts.
#
# Right-clicking either one picks that line and offers the inserts and the
# deletes.  Both
# are gestures Cellar installs by hand, because a heading is GtkColumnView's
# own widget and a gesture that has to claim an event sequence needs the
# gesture object in its own handler -- so neither is a thing the declarative
# markup can say, and neither is a thing the compiler can check.
#
# What is checked is the folder rather than the picture: an insert makes the
# sheet a row taller or a column wider, a delete makes it shorter or narrower,
# and the sheet file says so.
#
# The deletes are driven from the keyboard rather than from the menu, which
# checks the accelerators as well: a shortcut written with Shift and a
# punctuation key is the kind that parses and then never fires, because the
# keyval under Shift is a different one.
#
# Nothing here opens a dialog, which is what makes it worth trusting on a
# machine where the editor never receives what xdotool types.
#
# Run it from inside `nix develop`:
#
#     nix develop -c nix shell nixpkgs#xvfb-run nixpkgs#imagemagick \
#       nixpkgs#xdotool nixpkgs#dbus -c xvfb-run -s "-screen 0 1280x820x24" \
#       tests/gui-menu-smoke.sh

set -u
cd "$(dirname "$0")/.."

OUT="${OUT:-/tmp/cellar-menu-smoke}"
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
says () { grep -q "$2" "$1"; }

settle () {  # settle <seconds> <test...>
  local limit="$1"; shift
  local waited=0
  while [ "$waited" -lt "$limit" ]; do
    if "$@"; then return 0; fi
    sleep 2
    waited=$((waited + 2))
  done
  return 1
}

BOOK="$OUT/menus.cellar"
cellar_workbook "$BOOK" "Summary"
SHEET="$BOOK/sheets/Summary"
cellar_cell "$SHEET" A1 '"first"'

dbus-run-session -- "$CELLAR" "$BOOK" > "$OUT/app.log" 2>&1 &
APP=$!
trap 'kill $APP 2>/dev/null' EXIT

sleep 12
shot () { import -window root "$OUT/$1.png"; echo "  captured $1.png"; }
shot 1-before

# The menu opens under the pointer: the first item sits about 23px below the
# click and the items are 32px apart.
menu_item () {  # menu_item <click-x> <click-y> <item-number-from-one>
  local x="$1" y="$2" n="$3"
  xdotool mousemove "$x" "$y" click 3; sleep 3
  xdotool mousemove $(( x + 60 )) $(( y + 23 + (n - 1) * 32 )) click 1; sleep 3
}

echo "1. the menu on a row number"
# The row numbers are the narrow gutter down the left; row 2 is at y=205.
menu_item 35 205 1
shot 2-row-inserted
settle 20 says "$SHEET/sheet.scm" 'rows . 101'
expect "inserting a row from the gutter menu makes the sheet taller" \
  says "$SHEET/sheet.scm" 'rows . 101'

echo "2. the menu on a column heading"
# The headings sit at y=151; column B is around x=220.  The third item is
# Insert Column Before.
menu_item 220 151 3
shot 3-column-inserted
settle 20 says "$SHEET/sheet.scm" 'columns . 27'
expect "inserting a column from the heading menu makes the sheet wider" \
  says "$SHEET/sheet.scm" 'columns . 27'

echo "3. Ctrl+- deletes the active row"
# Click a cell first, so that the grid has the keyboard.
xdotool mousemove 220 205 click 1; sleep 2
xdotool key ctrl+minus; sleep 3
shot 4-row-deleted
settle 20 says "$SHEET/sheet.scm" 'rows . 100'
expect "Ctrl+- makes the sheet a row shorter" \
  says "$SHEET/sheet.scm" 'rows . 100'

echo "4. Ctrl+Alt+- deletes the active column"
xdotool key ctrl+alt+minus; sleep 3
shot 5-column-deleted
settle 20 says "$SHEET/sheet.scm" 'columns . 26'
expect "Ctrl+Alt+- makes the sheet a column narrower" \
  says "$SHEET/sheet.scm" 'columns . 26'

echo
echo "app log (excluding harmless environment noise):"
grep -av "libEGL\|DRI3\|dbus-daemon\|atk-bridge\|AT-SPI\|portal\|fusermount\|Registry\|display server" \
  "$OUT/app.log" | grep -av "^$" | head -5

echo
echo "screenshots in $OUT"
if [ "$failures" -eq 0 ]; then echo "ALL TESTS PASSED"; else
  echo "$failures FAILURE(S)"; exit 1
fi
