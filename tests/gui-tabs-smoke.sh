#!/usr/bin/env bash
#
# Drive the tabs: switching between the sheets of a workbook, adding one, and
# moving a workbook written before there were tabs into sheets/.
#
# The third companion to gui-smoke.sh, which exercises the grid, and
# gui-start-smoke.sh, which exercises the start page.  This one is about the
# workbook: several sheets in one folder, which is one Git repository.
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

failures=0
expect () {  # expect <description> <test...>
  local what="$1"; shift
  if "$@"; then echo "  ok   $what"; else echo "  FAIL $what"; failures=$((failures + 1)); fi
}
contains () { grep -q "$2" "$1"; }

# A workbook of three sheets, made through the store rather than by hand, so
# that what the app opens is what the app would have written.
WORKBOOK="$OUT/demo.cellar"
guile -L src -c '
(use-modules (cellar model) (cellar store))
(define workbook (list-ref (command-line) 1))
(create-workbook! workbook "Summary" #f)
(add-workbook-sheet! workbook "Q1")
(add-workbook-sheet! workbook "Q2")
(define (fill! name cells)
  (let ((sheet (make-sheet 100 26)))
    (for-each (lambda (cell)
                (set-cell-source! sheet (name->ref (car cell)) (cdr cell)))
              cells)
    (save-sheet! sheet (workbook-sheet-directory workbook name) (quote ()))))
(fill! "Summary" (quote (("A1" . "\"Summary sheet\""))))
(fill! "Q1" (quote (("A1" . "\"Q1 sheet\"") ("B1" . "1200"))))
(fill! "Q2" (quote (("A1" . "\"Q2 sheet\"") ("B1" . "2400"))))
(set-workbook-active! workbook "Q1")
' "$WORKBOOK" || { echo "could not build the workbook"; exit 1; }

# A workbook in the format from before tabs: sheet.scm and cells/ at the top,
# and no index above them.
LEGACY="$OUT/legacy.cellar"
cp -r example.cellar "$LEGACY"

# dbus-run-session, because GApplication is single-instance: with a Cellar
# already running on your session bus this one would hand its activation to
# that window and exit, leaving nothing here to photograph.
dbus-run-session -- guile -L src -s bin/cellar.scm "$WORKBOOK" > "$OUT/app.log" 2>&1 &
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
  contains "$WORKBOOK/workbook.scm" '(active . "Q1")'

# Ctrl+Page_Down and Ctrl+Page_Up walk the tabs.  Each sheet has its own model
# and its own grid, so what the window shows should change completely.
echo "2. moving between sheets"
xdotool key ctrl+Next; sleep 3
shot 2-next-sheet
expect "moving to a sheet is written down" \
  contains "$WORKBOOK/workbook.scm" '(active . "Q2")'

xdotool key ctrl+Prior ctrl+Prior; sleep 3
shot 3-first-sheet
expect "and so is moving back" \
  contains "$WORKBOOK/workbook.scm" '(active . "Summary")'

# Editing a cell writes it into that sheet's folder and no other.  Typing
# straight into the grid is not a thing Cellar does, so this goes through the
# editor dialog; where the dialog does not take the keys (see gui-smoke.sh)
# the check below is what says so.
echo "3. an edit lands in the sheet that is showing"
xdotool key Return; sleep 7
xdotool key ctrl+a; sleep 1
xdotool type --delay 30 '"edited on Summary"'
sleep 2
xdotool key ctrl+Return; sleep 4
shot 4-edited
expect "the cell went into the sheet that was showing" \
  contains "$WORKBOOK/sheets/Summary/cells/A1.scm" 'edited on Summary'
expect "and not into any other sheet" \
  contains "$WORKBOOK/sheets/Q1/cells/A1.scm" 'Q1 sheet'

# Ctrl+T adds a sheet.  The dialog suggests a name and Enter accepts it, so
# this needs nothing typed.
echo "4. adding a sheet"
xdotool key ctrl+t; sleep 4
shot 5-add-dialog
xdotool key Return; sleep 4
shot 6-added
expect "the new sheet has a folder" test -d "$WORKBOOK/sheets/Sheet 4"
expect "with a primary file in it" test -f "$WORKBOOK/sheets/Sheet 4/sheet.scm"
expect "and the index knows about it" \
  contains "$WORKBOOK/workbook.scm" '"Sheet 4"'

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
guile -L src -c '
(use-modules (cellar store))
(create-sheet-directory! (list-ref (command-line) 1) #f)
' "$WORKBOOK/sheets/FromDisk" || echo "  could not plant the sheet"
sleep 5
shot 7-arrived
for _ in 1 2 3 4 5 6 7 8; do
  xdotool key ctrl+Next
  sleep 4
  if contains "$WORKBOOK/workbook.scm" '(active . "FromDisk")'; then break; fi
done
shot 8-on-arrived
expect "the tab is there, and the keyboard reaches it" \
  contains "$WORKBOOK/workbook.scm" '(active . "FromDisk")'

echo "6. the sheets are all still there"
expect "Summary" test -d "$WORKBOOK/sheets/Summary"
expect "Q1" test -d "$WORKBOOK/sheets/Q1"
expect "Q2" test -d "$WORKBOOK/sheets/Q2"

kill $APP 2>/dev/null
wait $APP 2>/dev/null
sleep 2

# A workbook from before tabs opens where it lies, untouched, and is moved into
# sheets/ only when a second sheet gives it a reason to be.
echo "7. a workbook written before there were tabs"
dbus-run-session -- guile -L src -s bin/cellar.scm "$LEGACY" > "$OUT/legacy.log" 2>&1 &
APP=$!
sleep 12

xdotool mousemove 220 180 click 1; sleep 2
shot 9-legacy-opened
expect "it opened where it lies" test -f "$LEGACY/sheet.scm"
expect "and was not rearranged on the way in" test ! -d "$LEGACY/sheets"

echo "8. adding a sheet moves it into sheets/"
xdotool key ctrl+t; sleep 4
xdotool key Return; sleep 5
shot 10-legacy-migrated
expect "the old sheet moved under its own name" \
  test -f "$LEGACY/sheets/legacy/sheet.scm"
expect "and brought its cells with it" \
  test -f "$LEGACY/sheets/legacy/cells/A1.scm"
expect "the top of the workbook is clear" test ! -f "$LEGACY/sheet.scm"
expect "there is an index now" test -f "$LEGACY/workbook.scm"
expect "naming both sheets" contains "$LEGACY/workbook.scm" '"legacy"'
expect "the new sheet has a folder" test -d "$LEGACY/sheets/Sheet 2"

kill $APP 2>/dev/null

echo
echo "app log (excluding harmless EGL noise):"
grep -v "libEGL\|DRI3\|dbus-daemon\|atk-bridge\|AT-SPI\|portal\|fusermount\|Registry" \
  "$OUT/app.log" "$OUT/legacy.log" | head -20
echo "screenshots in $OUT"

if [ "$failures" -eq 0 ]; then
  echo "ALL TESTS PASSED"
else
  echo "$failures FAILURE(S)"
fi
exit "$failures"
