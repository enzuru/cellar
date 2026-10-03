#!/usr/bin/env bash
#
# Drive the tabs: switching between the sheets of a workbook, adding one, and
# picking up a sheet that arrived in the file from outside.
#
# The third companion to gui-smoke.sh, which exercises the grid, and
# gui-start-smoke.sh, which exercises the start page.  This one is about the
# workbook: several sheets in one file.
#
# Run it from inside `nix develop`:
#
#     nix develop -c nix shell nixpkgs#xvfb-run nixpkgs#imagemagick \
#       nixpkgs#xdotool nixpkgs#dbus -c xvfb-run -s "-screen 0 1280x820x24" \
#       tests/gui-tabs-smoke.sh
#
# Screenshots land in ${OUT:-/tmp/cellar-tabs-smoke}.
#
# Everything here is driven with the keyboard and with clicks on the grid.
# Nothing clicks a tab: with no window manager under Xvfb the tab strip's
# geometry depends on the theme's font, and a hard-coded coordinate would be
# a test of that rather than of Cellar.  Ctrl+Page_Down says the same thing
# and says it the same way on every machine.

set -u
cd "$(dirname "$0")/.."

OUT="${OUT:-/tmp/cellar-tabs-smoke}"
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
contains () { grep -q "$2" "$1"; }

# Wait for something to become true, up to a limit. Cellar's work is
# asynchronous now -- an edit goes to the kernel and the file is written when
# the kernel answers -- so a fixed sleep is either too short on a slow machine
# or wasted on a fast one. The assertions below are unchanged; only the waiting
# adapts.
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

# A workbook of three sheets, made through the store rather than by hand, so
# that what the app opens is what the app would have written.
WORKBOOK="$OUT/demo.cellar"
cellar_workbook "$WORKBOOK" Summary Q1 Q2
cellar_cell "$WORKBOOK" "Summary" A1 '"Summary sheet"'
cellar_cell "$WORKBOOK" "Q1" A1 '"Q1 sheet"'
cellar_cell "$WORKBOOK" "Q1" B1 '1200'
cellar_cell "$WORKBOOK" "Q2" A1 '"Q2 sheet"'
cellar_cell "$WORKBOOK" "Q2" B1 '2400'
cellar_active "$WORKBOOK" Q1

# and no index above them.

# dbus-run-session, because GApplication is single-instance: with a Cellar
# already running on your session bus this one would hand its activation to
# that window and exit, leaving nothing here to photograph.
dbus-run-session -- "$CELLAR" "$WORKBOOK" > "$OUT/app.log" 2>&1 &
APP=$!
trap 'kill $APP 2>/dev/null' EXIT

sleep 12

shot () { import -window root "$OUT/$1.png"; echo "  captured $1.png"; }

# The workbook opens on the sheet it was left on, which the index recorded as
# Q1 rather than the first tab.  Clicking the grid is also what gives the
# window keyboard focus, since there is no window manager under Xvfb.
echo "1. the workbook opens on the sheet it was left on"
xdotool mousemove 220 180 click 1; sleep 2
shot 1-opened
expect "the active sheet was remembered" \
  contains "$WORKBOOK" '(active . "Q1")'

# Ctrl+Page_Down and Ctrl+Page_Up walk the tabs.  Each sheet has its own model
# and its own grid, so what the window shows should change completely.
echo "2. moving between sheets"
xdotool key ctrl+Next; sleep 3
shot 2-next-sheet
expect "moving to a sheet is written down" \
  contains "$WORKBOOK" '(active . "Q2")'

xdotool key ctrl+Prior ctrl+Prior; sleep 3
shot 3-first-sheet
expect "and so is moving back" \
  contains "$WORKBOOK" '(active . "Summary")'

# Editing a cell writes it into that sheet's folder and no other.  Typing
# straight into the grid is not a thing Cellar does, so this goes through the
# editor dialog; where the dialog does not take the keys (see gui-smoke.sh)
# the check below is what says so.
echo "3. an edit lands in the sheet that is showing"
xdotool key Return; sleep 7
xdotool key ctrl+a; sleep 1
xdotool type --delay 30 '"edited on Summary"'
sleep 2
xdotool key ctrl+Return
settle 30 cellar_holds "$WORKBOOK" Summary A1 '"edited on Summary"'
shot 4-edited
expect "the cell went into the sheet that was showing" \
  cellar_holds "$WORKBOOK" Summary A1 '"edited on Summary"'
expect "and not into any other sheet" \
  cellar_holds "$WORKBOOK" Q1 A1 '"Q1 sheet"'

# Ctrl+T adds a sheet.  The dialog suggests a name and Enter accepts it, so
# this needs nothing typed.
echo "4. adding a sheet"
xdotool key ctrl+t; sleep 4
shot 5-add-dialog
xdotool key Return
settle 30 contains "$WORKBOOK" '"Sheet 4"'
shot 6-added
expect "the new sheet is in the file" contains "$WORKBOOK" '"Sheet 4"'
expect "with a size of its own" contains "$WORKBOOK" '(rows . 100)'
expect "and the index knows about it" \
  contains "$WORKBOOK" '"Sheet 4"'

# A sheet arriving from outside -- somebody else's commit, in practice. The
# workbook folder is watched, so the tabs are rebuilt without being asked. The
# proof that the new tab is really there is that the keyboard can reach it.
#
# This waits by trying rather than by sleeping. Cellar settles a burst of
# changes for a quarter of a second and then re-reads, which is all it takes
# where GIO has inotify to work with; under Xvfb in a sandbox it falls back to a
# polling monitor that has been seen to take the better part of ten seconds to
# notice a new folder. Ctrl+Page_Down does not wrap, so pressing it again on the
# last tab costs nothing, and the loop below stops the moment the rebuild lands.
echo "5. a sheet that arrived from outside"
cellar_add_sheet "$WORKBOOK" FromDisk
sleep 5
shot 7-arrived
# Press and re-check: the tab cannot be reached until the rebuild has landed,
# and Ctrl+Page_Down does not wrap, so pressing it again on the last tab costs
# nothing.
for _ in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
  xdotool key ctrl+Next
  sleep 4
  if contains "$WORKBOOK" '(active . "FromDisk")'; then break; fi
done
shot 8-on-arrived
expect "the tab is there, and the keyboard reaches it" \
  contains "$WORKBOOK" '(active . "FromDisk")'

echo "6. the sheets are all still there"
expect "Summary" contains "$WORKBOOK" '"Summary"'
expect "Q1" contains "$WORKBOOK" '"Q1"'
expect "Q2" contains "$WORKBOOK" '"Q2"'

kill $APP 2>/dev/null
wait $APP 2>/dev/null
sleep 2

echo
echo "app log (excluding harmless EGL noise):"
grep -v "libEGL\|DRI3\|dbus-daemon\|atk-bridge\|AT-SPI\|portal\|fusermount\|Registry" \
  "$OUT/app.log" | head -20
echo "screenshots in $OUT"

if [ "$failures" -eq 0 ]; then
  echo "ALL TESTS PASSED"
else
  echo "$failures FAILURE(S)"
fi
exit "$failures"
