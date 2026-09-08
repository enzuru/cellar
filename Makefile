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

.PHONY: all ui build run check check-shell check-kernel check-window coverage smoke clean

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

# The Guile half: the reference arithmetic it shares with the shell, the model
# it holds, and the protocol it answers on.
check-kernel:
	GUILE_AUTO_COMPILE=0 guile -L src -s tests/ref-test.scm
	GUILE_AUTO_COMPILE=0 guile -L src -s tests/model-test.scm
	GUILE_AUTO_COMPILE=0 guile -L src -s tests/kernel-test.scm

# The window, driven from code under a nested X server.
#
# Not part of `make check`, which needs neither the GTK bindings nor a display.
# This is the companion to the smoke scripts rather than a replacement for
# them: it calls what a signal handler would have called and asks the widgets
# what they say afterwards, so it needs no xdotool, no coordinates and no
# screenshots, while they keep the half of the story only a real keystroke can
# tell.
WINDOW_BIN := $(BUILD)/cellar-window-test

check-window: ui $(WINDOW_BIN)
	nix shell nixpkgs#xvfb-run nixpkgs#dbus \
	  -c xvfb-run -s "-screen 0 1280x820x24" \
	  dbus-run-session -- ./$(WINDOW_BIN)

$(WINDOW_BIN): $(HASKELL) test/Window.hs
	@mkdir -p $(BUILD)
	ghc -ihs -itest -outputdir $(BUILD)/window-objects -o $@ \
	  test/Window.hs -threaded $(WARNINGS)

# What the tests reach, and what they do not.
#
# A build of its own, instrumented with GHC's coverage counters, so that `make
# check` stays the fast gate.  Every module under hs/ is named on the command
# line rather than only the ones the suite imports: a module that is never
# imported is otherwise left out of the report altogether, and a figure that
# quietly omits the window would say four fifths of a program that is mostly
# untested.  Named that way they are linked in and reported at 0%, which is the
# truth.  hs/Main.hs is the exception -- it is a `Main` module, and the test
# suite is the `Main` of this binary.
#
# The report excludes the suite itself, since how much of the test file ran
# says nothing about the program.
COVERAGE := $(BUILD)/coverage
INSTRUMENTED := $(filter-out hs/Main.hs,$(HASKELL))

coverage:
	@mkdir -p $(COVERAGE)
	ghc -ihs -itest -fhpc -hpcdir $(COVERAGE)/mix \
	  -outputdir $(COVERAGE)/objects -o $(COVERAGE)/cellar-test \
	  test/Spec.hs $(INSTRUMENTED) -threaded $(WARNINGS)
	@# The counts from the last run were taken against the last build, and
	@# hpc refuses to mix the two.
	@rm -f $(COVERAGE)/cellar-test.tix
	GUILE_AUTO_COMPILE=0 HPCTIXFILE=$(COVERAGE)/cellar-test.tix \
	  ./$(COVERAGE)/cellar-test
	@echo
	@echo "the shell as a whole, the test suite itself left out:"
	@hpc report $(COVERAGE)/cellar-test.tix --hpcdir=$(COVERAGE)/mix \
	  --exclude=Main | sed 's/^/  /'
	@echo
	@echo "expressions run, by module:"
	@hpc report $(COVERAGE)/cellar-test.tix --hpcdir=$(COVERAGE)/mix \
	  --exclude=Main --per-module \
	  | awk '/^-----<module/ { name = $$2; sub(/>-----/, "", name) } \
	         /expressions used/ { printf "  %4s  %s\n", $$1, name }'

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
