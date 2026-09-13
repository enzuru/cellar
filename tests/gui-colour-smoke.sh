#!/usr/bin/env bash
#
# A cell that asks to be drawn in a colour of its own.
#
# A cell can say what colour it wants, and GTK has no way to set one on a
# widget except through a stylesheet, so Cellar collects the colours into
# classes and writes them into a provider of its own.  That is a piece of
# plumbing with nothing in the folder to show for it: the sheet on disk says
# what the cell is, not what it looks like, so the only place the answer lives
# is the screen.
#
# So this one reads the screen.  It opens a workbook whose first cell asks for
# a background, and looks at the pixel where that cell is drawn.
#
# Nothing here opens a dialog, which is what makes it worth trusting on a
# machine where the editor never receives what xdotool types.
#
# Run it from inside `nix develop`:
#
#     nix develop -c nix shell nixpkgs#xvfb-run nixpkgs#imagemagick \
#       nixpkgs#dbus -c xvfb-run -s "-screen 0 1280x820x24" \
#       tests/gui-colour-smoke.sh

set -u
cd "$(dirname "$0")/.."

OUT="${OUT:-/tmp/cellar-colour-smoke}"
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

# The workbook: one sheet, one cell that colours itself and one that does not.
BOOK="$OUT/colours.cellar"
cellar_workbook "$BOOK" "Summary"
SHEET="$BOOK/sheets/Summary"
cellar_cell "$SHEET" A1 '(styled 42 #:color "#ffffff" #:background "#3584e4")'
cellar_cell "$SHEET" A2 '7'

dbus-run-session -- "$CELLAR" "$BOOK" > "$OUT/app.log" 2>&1 &
APP=$!
trap 'kill $APP 2>/dev/null' EXIT

sleep 12
import -window root "$OUT/1-coloured.png"

# A1 is the first cell of the first column: x around 118, y around 180.  A2 is
# the row below it, which asked for nothing.
pixel () {  # pixel <x> <y>
  magick "$OUT/1-coloured.png" -format "%[pixel:p{$1,$2}]" info:
}

coloured=$(pixel 118 180)
plain=$(pixel 118 205)
echo "  A1 is $coloured, A2 is $plain"

# GTK draws the background through a stylesheet class, so the pixel is the
# colour the cell asked for rather than something near it.
is_blue () { [ "$coloured" = "srgb(53,132,228)" ] || [ "$coloured" = "#3584E4" ]; }
differs () { [ "$coloured" != "$plain" ]; }

expect "a cell drawn in the colour it asked for" is_blue
expect "and a cell that asked for nothing is not" differs

echo
echo "screenshot in $OUT"
if [ "$failures" -eq 0 ]; then echo "ALL TESTS PASSED"; else
  echo "$failures FAILURE(S)"; exit 1
fi
