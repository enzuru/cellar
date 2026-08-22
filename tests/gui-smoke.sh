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

# dbus-run-session, because GApplication is single-instance: with a Cellar
# already running on your session bus this one would hand its activation to
# that window and exit, leaving nothing here to photograph.
dbus-run-session -- guile -L src -s bin/cellar.scm example.cellar > "$OUT/app.log" 2>&1 &
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

# The application icon, resolved by application id out of data/icons. The menu
# button is at 957 -- 1070 is the close button, which quits the app instead.
echo "8. about dialog"
xdotool mousemove 957 27 click 1; sleep 3
shot 15-menu
# Click "About Cellar" by position. Never press Return here: the highlighted
# item is "Open...", and that summons the portal file chooser onto your real
# desktop.
xdotool mousemove 907 429 click 1; sleep 4
shot 16-about
xdotool key Escape; sleep 2

echo
echo "app log (excluding harmless EGL noise):"
grep -av "libEGL\|DRI3" "$OUT/app.log" | head -5
echo "screenshots in $OUT"
