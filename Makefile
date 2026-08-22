# Cellar -- build the Blueprint UI and run the app.
#
# Everything here assumes you are inside `nix develop`, which supplies Guile,
# G-Golf, blueprint-compiler and the GTK/libadwaita typelibs.

BLUEPRINTS := $(wildcard ui/*.blp)
UI := $(BLUEPRINTS:.blp=.ui)
SOURCES := $(wildcard src/cellar/*.scm)

.PHONY: all ui run check smoke clean

all: ui

ui: $(UI)

ui/%.ui: ui/%.blp
	blueprint-compiler compile --output $@ $<

run: ui
	GUILE_AUTO_COMPILE=0 guile -L src -s bin/cellar.scm $(FILE)

check:
	GUILE_AUTO_COMPILE=0 guile -L src -s tests/model-test.scm
	GUILE_AUTO_COMPILE=0 guile -L src -s tests/store-test.scm

# Drives the real UI under a nested X server; needs xvfb-run, imagemagick, xdotool.
smoke: ui
	nix shell nixpkgs#xvfb-run nixpkgs#imagemagick nixpkgs#xdotool nixpkgs#dbus \
	  -c xvfb-run -s "-screen 0 1280x820x24" tests/gui-smoke.sh
	nix shell nixpkgs#xvfb-run nixpkgs#imagemagick nixpkgs#xdotool nixpkgs#dbus \
	  -c xvfb-run -s "-screen 0 1280x820x24" tests/gui-start-smoke.sh

clean:
	rm -f $(UI)
	find . -name '*.go' -delete
