#!/usr/bin/env bash
#
# Drive the app under a nested X server and capture screenshots.
#
# Cellar has no display-free way to test the grid, so this clicks and types at
# it for real. Run it from inside `nix develop`:
#
#     nix develop -c nix shell nixpkgs#xvfb-run nixpkgs#imagemagick nixpkgs#xdotool \
#       -c xvfb-run -s "-screen 0 1280x820x24" tests/gui-smoke.sh
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

guile -L src -s bin/cellar.scm example.cellar > "$OUT/app.log" 2>&1 &
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

# Enter opens the editor on the active cell.
echo "3. editor"
xdotool key Return; sleep 4
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
xdotool mousemove 530 139 click --repeat 2 --delay 60 1; sleep 4
xdotool key ctrl+a; sleep 1
xdotool type --delay 30 '(/ 1 0)'; sleep 2
xdotool key ctrl+Return; sleep 4
shot 6-error

# The application icon, resolved by application id out of data/icons. The menu
# button is at 957 -- 1070 is the close button, which quits the app instead.
echo "6. about dialog"
xdotool mousemove 957 27 click 1; sleep 3
shot 7-menu
# Click "About Cellar" by position. Never press Return here: the highlighted
# item is "Open...", and that summons the portal file chooser onto your real
# desktop.
xdotool mousemove 907 297 click 1; sleep 4
shot 8-about
xdotool key Escape; sleep 2

echo
echo "app log (excluding harmless EGL noise):"
grep -av "libEGL\|DRI3" "$OUT/app.log" | head -5
echo "screenshots in $OUT"
