# Cellar -- build the Blueprint UI, build the Haskell shell, and run the pair.
#
# Everything here assumes you are inside `nix develop`, which supplies GHC with
# the haskell-gi bindings, Guile for the kernel, blueprint-compiler, and the
# GTK/libadwaita libraries.
#
# Cellar is two programs.  `make build` compiles the shell; the kernel is a
# Guile script and needs no compiling to be run.

BLUEPRINTS := $(wildcard ui/*.blp)
UI := $(BLUEPRINTS:.blp=.ui)
HASKELL := $(shell find hs -name '*.hs')

# The same set the cabal file asks for, so that `make build` and a cabal build
# disagree about nothing.
WARNINGS := -Wall -Wcompat -Wincomplete-record-updates \
            -Wincomplete-uni-patterns -Wredundant-constraints

BUILD := .build
SHELL_BIN := $(BUILD)/cellar

.PHONY: all ui build run check check-shell check-kernel smoke clean

all: ui build

ui: $(UI)

ui/%.ui: ui/%.blp
	blueprint-compiler compile --output $@ $<

build: $(SHELL_BIN)

$(SHELL_BIN): $(HASKELL)
	@mkdir -p $(BUILD)
	ghc -ihs -outputdir $(BUILD)/objects -o $@ hs/Main.hs -threaded $(WARNINGS)

run: ui build
	./$(SHELL_BIN) $(FILE)

# Both halves are tested without a display.
check: check-shell check-kernel

# The shell: references, s-expressions, framing, the store, views, the
# preferences, and the client driving a real Guile kernel over a real pipe.
check-shell:
	@mkdir -p $(BUILD)
	ghc -ihs -itest -outputdir $(BUILD)/test-objects -o $(BUILD)/cellar-test \
	  test/Spec.hs -threaded $(WARNINGS)
	GUILE_AUTO_COMPILE=0 ./$(BUILD)/cellar-test

# The kernel: the model it holds, and the protocol it answers on.
check-kernel:
	GUILE_AUTO_COMPILE=0 guile -L src -s tests/model-test.scm
	GUILE_AUTO_COMPILE=0 guile -L src -s tests/kernel-test.scm

# Drives the real UI under a nested X server; needs xvfb-run, imagemagick, xdotool.
smoke: ui build
	nix shell nixpkgs#xvfb-run nixpkgs#imagemagick nixpkgs#xdotool nixpkgs#dbus \
	  -c xvfb-run -s "-screen 0 1280x820x24" tests/gui-smoke.sh
	nix shell nixpkgs#xvfb-run nixpkgs#imagemagick nixpkgs#xdotool nixpkgs#dbus \
	  -c xvfb-run -s "-screen 0 1280x820x24" tests/gui-start-smoke.sh
	nix shell nixpkgs#xvfb-run nixpkgs#imagemagick nixpkgs#xdotool nixpkgs#dbus \
	  -c xvfb-run -s "-screen 0 1280x820x24" tests/gui-tabs-smoke.sh
	nix shell nixpkgs#xvfb-run nixpkgs#imagemagick nixpkgs#xdotool nixpkgs#dbus \
	  -c xvfb-run -s "-screen 0 1280x820x24" tests/gui-kernel-smoke.sh
	nix shell nixpkgs#xvfb-run nixpkgs#imagemagick nixpkgs#xdotool nixpkgs#dbus \
	  -c xvfb-run -s "-screen 0 1280x820x24" tests/gui-drag-smoke.sh
	nix shell nixpkgs#xvfb-run nixpkgs#imagemagick nixpkgs#xdotool nixpkgs#dbus \
	  -c xvfb-run -s "-screen 0 1280x820x24" tests/gui-editor-smoke.sh

clean:
	rm -f $(UI)
	rm -rf $(BUILD)
	find . -name '*.go' -delete
