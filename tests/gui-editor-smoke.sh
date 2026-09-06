#!/usr/bin/env bash
#
# Drive the external editor, and the preference that names it.
#
# The preference is set the way a person would have left it -- in the config
# file -- rather than by typing into the dialog, because that would be a test of
# where a row is on screen. What is being checked is the path from that setting
# to a cell on disk: Cellar reads the command, hands the cell's own file to it,
# and the folder watcher brings the edit back.
#
# The last step is the one that matters. After the editor has written the file,
# Cellar is made to rewrite the whole sheet from what it is holding. If the
# watcher had missed the edit, that would put the old value back; the new one
# surviving is the proof that it did not.
#
# Run it from inside `nix develop`:
#
#     nix develop -c nix shell nixpkgs#xvfb-run nixpkgs#imagemagick \
#       nixpkgs#xdotool nixpkgs#dbus -c xvfb-run -s "-screen 0 1280x820x24" \
#       tests/gui-editor-smoke.sh
#
# Screenshots land in ${OUT:-/tmp/cellar-editor-smoke}.

set -u
cd "$(dirname "$0")/.."

OUT="${OUT:-/tmp/cellar-editor-smoke}"
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

WORKBOOK="$OUT/editing.cellar"
SHEET="$WORKBOOK/sheets/Summary"
cellar_workbook "$WORKBOOK" Summary
cellar_cell "$SHEET" A1 '"before"'
cellar_cell "$SHEET" A2 '"second"'
cellar_cell "$SHEET" B1 'A1'

# The stand-in editor: it writes the file it is handed and exits, which is
# everything Cellar asks of a real one.
EDITOR_SCRIPT="$OUT/stand-in-editor"
cat > "$EDITOR_SCRIPT" <<'EOF'
#!/bin/sh
printf '"after"\n' > "$1"
EOF
chmod +x "$EDITOR_SCRIPT"

# The preference, as somebody would have left it.  A command here is what Open
# runs; without one it would be whatever the desktop opens text files with,
# which under Xvfb is nothing at all.
export CELLAR_CONFIG="$OUT/config.scm"
cat > "$CELLAR_CONFIG" <<EOF
;; Cellar preferences.
((external-editor-command . "$EDITOR_SCRIPT"))
EOF

dbus-run-session -- "$CELLAR" "$WORKBOOK" > "$OUT/app.log" 2>&1 &
APP=$!
trap 'kill $APP 2>/dev/null; pkill -f "[.]build/cellar" 2>/dev/null' EXIT

sleep 15

shot () { import -window root "$OUT/$1.png"; echo "  captured $1.png"; }

xdotool mousemove 118 180 click 1; sleep 2
shot 1-opened

echo "1. the preferences dialog opens"
xdotool key ctrl+comma; sleep 4
shot 2-preferences
expect "it drew something" test -s "$OUT/2-preferences.png"
xdotool key Escape; sleep 2

echo "2. a cell goes to the external editor"
# Ctrl+Shift+E opens the active cell elsewhere -- the command above, since the
# preference names one -- while Ctrl+E is Cellar's own editor. No dialog opens
# either way here, so nothing in this script has to type into one.
# Column A, row 1: the cell the assertions below are about.
xdotool mousemove 118 180 click 1; sleep 1
xdotool key ctrl+shift+e
settle 30 holds "$SHEET/cells/A1.scm" '"after"'
shot 3-edited
expect "the editor wrote the cell's own file" holds "$SHEET/cells/A1.scm" '"after"'

echo "3. and Cellar took it in"
# Insert a row: that makes Cellar rewrite every cell from what it is holding.
# If the watcher had missed the edit, this would put "before" back.
xdotool key ctrl+alt+Down
settle 30 holds "$SHEET/cells/A1.scm" '"after"'
shot 4-after-rewrite
expect "the edit survived a whole-sheet write" \
  holds "$SHEET/cells/A1.scm" '"after"'
expect "and the sheet really was rewritten" test -f "$SHEET/cells/A3.scm"

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
