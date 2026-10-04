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

# GHC optimises nothing unless it is asked, and for a long time this Makefile
# did not ask, so the program people ran was the unoptimised one.  `cabal
# build' asks for -O on its own, which is why only the Makefile build was
# affected, and why nobody noticed.
#
# The same scripted session -- a workbook opened and the selection moved 600
# times -- measured at each level:
#
#   -O0   0.68 s   431 MB allocated     clean build 31 s
#   -O    0.41 s   237 MB allocated     clean build 71 s
#   -O2   0.38 s   232 MB allocated     clean build 84 s
#
# So most of it is -O, and -O2 is thirteen seconds of compiling for about two
# percent less allocation.  The test builds below are left alone: they are the
# gate, there are three of them, and they are compiled far more often than
# they are slow.
OPTIMISATION := -O2

BUILD := .build
SHELL_BIN := $(BUILD)/cellar

.PHONY: all ui build run check check-shell check-properties check-kernel \
        check-window coverage profile profile-heap smoke clean

all: ui build

ui: $(UI)

ui/%.ui: ui/%.blp
	blueprint-compiler compile --output $@ $<

build: $(SHELL_BIN)

$(SHELL_BIN): $(SOURCES)
	@mkdir -p $(BUILD)
	ghc $(INCLUDES) $(OPTIMISATION) -outputdir $(BUILD)/objects -o $@ \
	  hs/Main.hs -threaded $(WARNINGS)

run: ui build
	./$(SHELL_BIN) $(FILE)

# Every one of these runs without a display.
check: check-shell check-properties check-window check-kernel

# The shell: references, s-expressions, framing, the store, views, the
# preferences, and the client driving a real Guile kernel over a real pipe.
check-shell:
	@mkdir -p $(BUILD)
	ghc $(INCLUDES) -itest -outputdir $(BUILD)/test-objects -o $(BUILD)/cellar-test \
	  test/Spec.hs -threaded $(WARNINGS)
	GUILE_AUTO_COMPILE=0 ./$(BUILD)/cellar-test

# The laws, against generated input.
#
# Separate from check-shell because it is a different kind of test and reads as
# one: that suite says what the rules are, an example each, and this says they
# hold.  It is built with the library on the search path because one of the
# laws is about the grid, and it starts Guile because the law worth most is
# that the reference arithmetic written here and the copy in src/cellar/ref.scm
# agree.  It runs from the top of the repository, which is how `guile -L src'
# finds that copy.
PROPERTIES_BIN := $(BUILD)/cellar-properties

check-properties: $(PROPERTIES_BIN)
	GUILE_AUTO_COMPILE=0 ./$(PROPERTIES_BIN)

$(PROPERTIES_BIN): $(SOURCES) test/Properties.hs
	@mkdir -p $(BUILD)
	ghc $(INCLUDES) -itest -outputdir $(BUILD)/properties-objects -o $@ \
	  test/Properties.hs -threaded $(WARNINGS)

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

# Where the time goes, from a real session.
#
# A build of its own, like the coverage one, so that `make build' stays the
# fast gate.  It covers Cellar and gi-gtk4-declarative together, because the
# Makefile compiles the library from source alongside Cellar, which is exactly
# what makes the library's own costs visible here.
#
# -fprof-late rather than -fprof-auto: the cost centres go in after
# optimisation, so the program that runs is the program that ships.
# -fprof-auto inserts them first and blocks the inlining that would otherwise
# happen, which measures a different program and blames the wrong things.
#
# It is optimised for the same reason, and that is not a detail.  The first
# profile taken here was of an -O0 build, and it blamed class dictionaries
# being passed at runtime for an eighth of the time -- which is a true thing
# to say about an unoptimised program and says nothing at all about the one
# that ships.  Profile the program that runs.
#
# The profile is written when the program exits, so quit with Ctrl+Q rather
# than killing the window, or there will be nothing to read.
PROFILE_BIN := $(BUILD)/cellar-prof

profile: ui $(PROFILE_BIN)
	./$(PROFILE_BIN) $(FILE) +RTS -p -s -RTS
	@echo
	@echo "wrote cellar-prof.prof -- the twenty costliest entries:"
	@sed -n '/^COST CENTRE/,$$p' cellar-prof.prof | head -22

# The same build, reporting what is on the heap rather than where the time
# went.  -hc groups what is live by the cost centre that allocated it.
profile-heap: ui $(PROFILE_BIN)
	./$(PROFILE_BIN) $(FILE) +RTS -hc -p -s -RTS
	@echo
	@echo "wrote cellar-prof.hp and cellar-prof.prof"

$(PROFILE_BIN): $(SOURCES)
	@mkdir -p $(BUILD)
	ghc $(INCLUDES) $(OPTIMISATION) -prof -fprof-late \
	  -outputdir $(BUILD)/prof-objects -o $@ \
	  hs/Main.hs -threaded -rtsopts $(WARNINGS)

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
	ghc $(INCLUDES) -itest -fhpc -hpcdir $(COVERAGE)/mix \
	  -outputdir $(COVERAGE)/properties-objects -o $(COVERAGE)/cellar-properties \
	  test/Properties.hs $(INSTRUMENTED) -threaded $(WARNINGS)
	@# The counts from the last run were taken against the last build, and
	@# hpc refuses to mix the two.
	@rm -f $(COVERAGE)/*.tix
	GUILE_AUTO_COMPILE=0 HPCTIXFILE=$(COVERAGE)/shell.tix \
	  ./$(COVERAGE)/cellar-test
	GUILE_AUTO_COMPILE=0 HPCTIXFILE=$(COVERAGE)/window.tix \
	  ./$(COVERAGE)/cellar-window-test
	GUILE_AUTO_COMPILE=0 HPCTIXFILE=$(COVERAGE)/properties.tix \
	  ./$(COVERAGE)/cellar-properties
	@# All three suites, counted together.  Each is its own program with a
	@# Main of its own, which is the one module they cannot share, and which
	@# the report leaves out anyway.
	@hpc sum --union --exclude=Main --output=$(COVERAGE)/both.tix \
	  $(COVERAGE)/shell.tix $(COVERAGE)/window.tix $(COVERAGE)/properties.tix
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

# Drives the real UI under a nested X server.  xvfb-run, xdotool, ImageMagick
# and dbus come from the dev shell, so that one GC root over the shell keeps
# them: fetching them per run with `nix shell nixpkgs#...' left them unrooted,
# and a garbage collection took them away twice in one week.
smoke: ui build
	xvfb-run -s "-screen 0 1280x820x24" tests/gui-smoke.sh
	xvfb-run -s "-screen 0 1280x820x24" tests/gui-start-smoke.sh
	xvfb-run -s "-screen 0 1280x820x24" tests/gui-tabs-smoke.sh
	xvfb-run -s "-screen 0 1280x820x24" tests/gui-kernel-smoke.sh
	xvfb-run -s "-screen 0 1280x820x24" tests/gui-drag-smoke.sh
	xvfb-run -s "-screen 0 1280x820x24" tests/gui-editor-smoke.sh
	xvfb-run -s "-screen 0 1280x820x24" tests/gui-colour-smoke.sh
	xvfb-run -s "-screen 0 1280x820x24" tests/gui-menu-smoke.sh

clean:
	rm -f $(UI)
	rm -rf $(BUILD)
	find . -name '*.go' -delete
