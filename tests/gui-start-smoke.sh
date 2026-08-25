#!/usr/bin/env bash
#
# Drive the start page: making a workbook, a scratch workbook, and saving one.
#
# The companion to gui-smoke.sh, which starts with a workbook already open and
# exercises the grid. This one starts with nothing open, which is what the
# application now does when it is given nothing to open.
#
# Run it from inside `nix develop`:
#
#     nix develop -c nix shell nixpkgs#xvfb-run nixpkgs#imagemagick \
#       nixpkgs#xdotool nixpkgs#dbus -c xvfb-run -s "-screen 0 1280x820x24" \
#       tests/gui-start-smoke.sh
#
# Screenshots land in ${OUT:-/tmp/cellar-start-smoke}.
#
# KNOWN ISSUE: from step 3 on, this suite does not currently drive every
# machine. Where it fails, the cell editor opens but never receives what
# xdotool types, so the edit -- and every keystroke after it, the modal dialog
# swallowing them -- does nothing. It fails identically on the commit before
# per-cell saving was added, so it is the harness meeting this environment and
# not the application: gui-smoke.sh covers the same editing paths and passes.
# Opening the editor with Return rather than a double-click was seen to work
# where the double-click did not, which is the thread to pull on next.
#
# WARNING: do not add a step that clicks "Open Workbook…" or the location
# button in the New Workbook dialog. Both hand off to the XDG desktop portal,
# whose window appears on your REAL desktop and cannot be scripted from here.
# Every other path through this screen stays inside Xvfb, which is why the New
# Workbook dialog defaults its location to $HOME rather than making you pick
# one.

set -u
cd "$(dirname "$0")/.."

OUT="${OUT:-/tmp/cellar-start-smoke}"
rm -rf "$OUT"
mkdir -p "$OUT"

# A HOME of its own: the New Workbook dialog puts workbooks there by default,
# and a test has no business writing into the real one.
export HOME="$OUT/home"
mkdir -p "$HOME"

export GDK_BACKEND=x11 GSK_RENDERER=cairo GUILE_AUTO_COMPILE=0

failures=0
expect () {  # expect <description> <test...>
  local what="$1"; shift
  if "$@"; then echo "  ok   $what"; else echo "  FAIL $what"; failures=$((failures + 1)); fi
}
contains () { grep -q "$2" "$1"; }

dbus-run-session -- guile -L src -s bin/cellar.scm > "$OUT/app.log" 2>&1 &
APP=$!
trap 'kill $APP 2>/dev/null' EXIT

sleep 12

shot () { import -window root "$OUT/$1.png"; echo "  captured $1.png"; }

echo "1. the start page"
shot 1-start

# New Workbook: a name, a location already filled in, and git on by default.
echo "2. new workbook"
xdotool mousemove 549 527 click 1; sleep 3
shot 2-new-dialog
# Click into the name entry: nothing in the dialog has the keyboard yet.
xdotool mousemove 549 339 click 1; sleep 1
xdotool key ctrl+a; sleep 1
xdotool type --delay 30 'budget'
sleep 1
shot 3-named
# "Create" is the suggested response, on the right.
xdotool key Return; sleep 7
shot 4-created

SHEET="$HOME/budget.cellar/sheets/Sheet 1"

expect "the workbook folder was made" test -d "$HOME/budget.cellar"
expect "with an index" test -f "$HOME/budget.cellar/workbook.scm"
expect "naming its first sheet" contains "$HOME/budget.cellar/workbook.scm" '"Sheet 1"'
expect "which has a folder" test -d "$SHEET"
expect "and a primary file" test -f "$SHEET/sheet.scm"
expect "and somewhere for the cells" test -d "$SHEET/cells"
expect "and a git repository around the whole workbook" test -d "$HOME/budget.cellar/.git"

# A cell, written the ordinary way. Nothing is saved afterwards, because there
# is nothing to save: applying the edit is what put it on disk.
echo "3. write a cell, and find it on disk without saving anything"
xdotool mousemove 220 164 click --repeat 2 --delay 60 1; sleep 4
xdotool type --delay 30 '(* 6 7)'
sleep 2
xdotool key ctrl+Return; sleep 3
shot 5-cell-written

expect "the cell is a file of its own" test -f "$SHEET/cells/B2.scm"
expect "holding its source" contains "$SHEET/cells/B2.scm" '(\* 6 7)'
expect "and no file for a cell that holds nothing" test ! -f "$SHEET/cells/A1.scm"
expect "the primary file knows the size" contains "$SHEET/sheet.scm" 'rows . 100'
expect "and has a place for column widths" contains "$SHEET/sheet.scm" 'widths'

# Inserting a row changes the shape of the sheet, which is the primary file's
# business rather than any cell's, and it reaches disk on its own too.
echo "4. a grown sheet is grown on disk"
xdotool key ctrl+alt+Down; sleep 3
expect "the sheet is a row taller on disk" contains "$SHEET/sheet.scm" 'rows . 101'

# Ctrl+S has nothing left to do, and says so rather than doing nothing quietly.
echo "5. Ctrl+S explains itself"
xdotool key ctrl+s; sleep 2
shot 6-ctrl-s

# A scratch workbook lives in a folder of Cellar's choosing, so it saves itself
# from the first keystroke like any other.
echo "6. a scratch workbook"
xdotool key ctrl+shift+n; sleep 4
shot 7-scratch
xdotool mousemove 220 164 click --repeat 2 --delay 60 1; sleep 4
xdotool type --delay 30 '(+ 40 2)'
sleep 2
xdotool key ctrl+Return; sleep 4
shot 8-scratch-written

SCRATCH="$HOME/.local/share/cellar/scratch"
expect "the scratch workbook was given a folder" test -d "$SCRATCH"
expect "and its cell is on disk, unasked" \
  bash -c 'grep -rq "40 2" "$1"/*/sheets' _ "$SCRATCH"

echo
echo "app log (excluding harmless environment noise):"
grep -av "libEGL\|DRI3\|atspi\|AT-SPI\|atk-bridge\|portal\|dbus-daemon\|gvfs\|display server" \
  "$OUT/app.log" | head -5
echo "screenshots in $OUT"
if [ "$failures" -gt 0 ]; then echo "$failures FAILURE(S)"; exit 1; fi
echo "ALL CHECKS PASSED"
