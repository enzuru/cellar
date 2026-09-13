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

# gi-gtk4-declarative, the declarative layer the window is being moved onto.
#
# It is not published anywhere yet, so it is compiled from a checkout beside
# this one rather than named as a package.  Point DECLARATIVE somewhere else
# if yours is not there.  The compiler builds those sources along with
# Cellar's, which is why the dev shell carries their dependencies -- see
# flake.nix.
DECLARATIVE ?= ../gi-gtk-declarative
DECLARATIVE_DIRS := $(DECLARATIVE)/gi-gtk4-declarative/src \
                    $(DECLARATIVE)/gi-gtk4-declarative-app-simple/src \
                    $(DECLARATIVE)/gi-gtk4-declarative-adwaita/src
DECLARATIVE_INCLUDES := $(addprefix -i,$(DECLARATIVE_DIRS))
DECLARATIVE_SOURCES := $(shell find $(DECLARATIVE_DIRS) -name '*.hs' 2>/dev/null)

# The search path every compiler call below uses: Cellar's own modules, then
# the library's.
INCLUDES := -ihs $(DECLARATIVE_INCLUDES)
SOURCES := $(HASKELL) $(DECLARATIVE_SOURCES)

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

$(SHELL_BIN): $(SOURCES)
	@mkdir -p $(BUILD)
	ghc $(INCLUDES) -outputdir $(BUILD)/objects -o $@ hs/Main.hs -threaded $(WARNINGS)

run: ui build
	./$(SHELL_BIN) $(FILE)

# Every one of these runs without a display.
check: check-shell check-window check-kernel

# The shell: references, s-expressions, framing, the store, views, the
# preferences, and the client driving a real Guile kernel over a real pipe.
check-shell:
	@mkdir -p $(BUILD)
	ghc $(INCLUDES) -itest -outputdir $(BUILD)/test-objects -o $(BUILD)/cellar-test \
	  test/Spec.hs -threaded $(WARNINGS)
	GUILE_AUTO_COMPILE=0 ./$(BUILD)/cellar-test

# The Guile half: the reference arithmetic it shares with the shell, the model
# it holds, and the protocol it answers on.
check-kernel:
	GUILE_AUTO_COMPILE=0 guile -L src -s tests/ref-test.scm
	GUILE_AUTO_COMPILE=0 guile -L src -s tests/model-test.scm
	GUILE_AUTO_COMPILE=0 guile -L src -s tests/kernel-test.scm

# The window, driven from code.
#
# The window is a function of one value now, so this hands events to the update
# and looks at the value that comes back -- with the real kernel over a real
# pipe and the real folder on disk, and with no display at all, which is why it
# is part of `make check' rather than a target of its own with an X server
# behind it.  The smoke scripts under tests/ are the other half: they drive the
# widgets, which this deliberately does not.
WINDOW_BIN := $(BUILD)/cellar-window-test

check-window: $(WINDOW_BIN)
	GUILE_AUTO_COMPILE=0 ./$(WINDOW_BIN)

$(WINDOW_BIN): $(SOURCES) test/Window.hs
	@mkdir -p $(BUILD)
	ghc $(INCLUDES) -itest -outputdir $(BUILD)/window-objects -o $@ \
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
	ghc $(INCLUDES) -itest -fhpc -hpcdir $(COVERAGE)/mix \
	  -outputdir $(COVERAGE)/objects -o $(COVERAGE)/cellar-test \
	  test/Spec.hs $(INSTRUMENTED) -threaded $(WARNINGS)
	ghc $(INCLUDES) -itest -fhpc -hpcdir $(COVERAGE)/mix \
	  -outputdir $(COVERAGE)/window-objects -o $(COVERAGE)/cellar-window-test \
	  test/Window.hs $(INSTRUMENTED) -threaded $(WARNINGS)
	@# The counts from the last run were taken against the last build, and
	@# hpc refuses to mix the two.
	@rm -f $(COVERAGE)/*.tix
	GUILE_AUTO_COMPILE=0 HPCTIXFILE=$(COVERAGE)/shell.tix \
	  ./$(COVERAGE)/cellar-test
	GUILE_AUTO_COMPILE=0 HPCTIXFILE=$(COVERAGE)/window.tix \
	  ./$(COVERAGE)/cellar-window-test
	@# Both suites, counted together.  Each is its own program with a Main
	@# of its own, which is the one module they cannot share, and which the
	@# report leaves out anyway.
	@hpc sum --union --exclude=Main --output=$(COVERAGE)/both.tix \
	  $(COVERAGE)/shell.tix $(COVERAGE)/window.tix
	@# The declarative library is compiled from source along with Cellar, so
	@# it is instrumented along with it.  It has a test suite of its own and
	@# this is not it, so it is left out of both reports.
	@echo
	@echo "Cellar, the test suites and the library left out:"
	@hpc report $(COVERAGE)/both.tix --hpcdir=$(COVERAGE)/mix --exclude=Main \
	  `find $(COVERAGE)/mix -name 'GI.*.mix' -o -name 'Pipes*.mix' \
	     | sed 's#.*/##; s#\.mix$$##; s#^#--exclude=#'` | sed 's/^/  /'
	@echo
	@echo "expressions run, by module:"
	@hpc report $(COVERAGE)/both.tix --hpcdir=$(COVERAGE)/mix --exclude=Main \
	  --per-module `find $(COVERAGE)/mix -name 'GI.*.mix' -o -name 'Pipes*.mix' \
	     | sed 's#.*/##; s#\.mix$$##; s#^#--exclude=#'` \
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
	nix shell nixpkgs#xvfb-run nixpkgs#imagemagick nixpkgs#dbus \
	  -c xvfb-run -s "-screen 0 1280x820x24" tests/gui-colour-smoke.sh

clean:
	rm -f $(UI)
	rm -rf $(BUILD)
	find . -name '*.go' -delete
