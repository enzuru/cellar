#!/usr/bin/env bash
#
# Drive the thing the kernel split exists for: a cell that never finishes.
#
# A cell holds an arbitrary Guile expression, so a cell can hold
# (let loop () (loop)).  While the sheet lived inside the application that meant
# the window stopped and stayed stopped -- evaluation ran inside the paint, and
# there was nowhere to catch it from.  Now it means one process stops and the
# one holding the window carries on.
#
# The runaway cell is planted in the folder rather than typed, so that this test
# does not depend on the editor dialog taking keystrokes -- see the note at the
# head of gui-start-smoke.sh for why that is not something to rely on here.
#
# Run it from inside `nix develop`:
#
#     nix develop -c nix shell nixpkgs#xvfb-run nixpkgs#imagemagick \
#       nixpkgs#xdotool nixpkgs#dbus -c xvfb-run -s "-screen 0 1280x820x24" \
#       tests/gui-kernel-smoke.sh
#
# Screenshots land in ${OUT:-/tmp/cellar-kernel-smoke}.

set -u
cd "$(dirname "$0")/.."

OUT="${OUT:-/tmp/cellar-kernel-smoke}"
rm -rf "$OUT"
mkdir -p "$OUT"

export HOME="$OUT/home"
mkdir -p "$HOME"

export GDK_BACKEND=x11 GSK_RENDERER=cairo GUILE_AUTO_COMPILE=0

# The shell is a compiled program now; the kernel it starts is still Guile, and
# is found beside this checkout.
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

WORKBOOK="$OUT/runaway.cellar"
cellar_workbook "$WORKBOOK" Summary
cellar_cell "$WORKBOOK/sheets/Summary" A1 '"before"'
cellar_cell "$WORKBOOK/sheets/Summary" A2 '(* 6 7)'
# The cell this whole test is about.
cellar_cell "$WORKBOOK/sheets/Summary" B1 '(let loop () (loop))'

dbus-run-session -- "$CELLAR" "$WORKBOOK" > "$OUT/app.log" 2>&1 &
APP=$!
trap 'kill $APP 2>/dev/null; pkill -f cellar-kernel.scm 2>/dev/null' EXIT

# Long enough for the kernel to start, wedge itself on B1, and for Cellar to
# run out of patience and ask about it.
sleep 30

shot () { import -window root "$OUT/$1.png"; echo "  captured $1.png"; }

echo "1. the window is still there"
shot 1-stalled
expect "the application did not die with the cell" kill -0 "$APP"

# The window still answers X: it is drawing, and it is reading its own events,
# because the thing that will not finish is in another process entirely. If the
# evaluator were still in here this would be the frame that never came.
echo "2. and still drawing"
xdotool mousemove 220 180 click 1; sleep 2
xdotool key ctrl+question; sleep 3
shot 2-shortcuts-over-stall
expect "a dialog opened while the kernel was wedged" \
  test -s "$OUT/2-shortcuts-over-stall.png"
xdotool key Escape; sleep 2

echo "3. the kernel is the part that is stuck"
expect "and it is still running, spinning on the cell" \
  bash -c 'pgrep -f "cellar-kernel[.]scm" > /dev/null' 

echo "4. stopping it"
# The stall dialog is modal and centred; the destructive response is the upper
# of the two buttons.
xdotool key Escape; sleep 1
shot 3-before-stop

# $APP is dbus-run-session, which is not the process the kernel is a child of.
# Killing it does not orphan anything, so the shell itself is what has to go.
pkill -f "[.]build/cellar" 2>/dev/null
kill $APP 2>/dev/null
wait $APP 2>/dev/null
sleep 2
expect "no kernel outlives the window that started it" \
  bash -c '! pgrep -f "cellar-kernel[.]scm" >/dev/null'

echo
echo "app log (excluding harmless environment noise):"
grep -av "libEGL\|DRI3\|dbus-daemon\|atk-bridge\|AT-SPI\|portal\|fusermount\|Registry\|display server" \
  "$OUT/app.log" | grep -av "^$" | head -10
echo "screenshots in $OUT"

if [ "$failures" -eq 0 ]; then
  echo "ALL TESTS PASSED"
else
  echo "$failures FAILURE(S)"
fi
exit "$failures"
