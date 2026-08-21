# Cellar -- build the Blueprint UI and run the app.
#
# Everything here assumes you are inside `nix develop`, which supplies Guile,
# G-Golf, blueprint-compiler and the GTK/libadwaita typelibs.

BLUEPRINTS := $(wildcard ui/*.blp)
UI := $(BLUEPRINTS:.blp=.ui)
SOURCES := $(wildcard src/cellar/*.scm)

.PHONY: all ui run check clean

all: ui

ui: $(UI)

ui/%.ui: ui/%.blp
	blueprint-compiler compile --output $@ $<

run: ui
	GUILE_AUTO_COMPILE=0 guile -L src -s bin/cellar.scm $(FILE)

check:
	GUILE_AUTO_COMPILE=0 guile -L src -s tests/model-test.scm

clean:
	rm -f $(UI)
	find . -name '*.go' -delete
