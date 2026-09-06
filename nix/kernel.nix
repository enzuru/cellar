{ lib, stdenv, makeWrapper, guile }:

# The kernel half of Cellar: the sheets and the evaluator.
#
# It is a Guile program and nothing else -- no GTK, no introspection, no
# g-golf.  That is the point of the split, and it shows up here as a derivation
# with almost nothing in it.

let
  guileVersion = lib.versions.majorMinor guile.version;
in
stdenv.mkDerivation {
  pname = "cellar-kernel";
  version = "0.1.0";

  src = ../.;

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ guile ];

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/cellar-kernel $out/bin

    # Only the modules the kernel actually uses.  The shell's Guile modules are
    # gone; what is left is the model, the references it names cells by, the
    # wire format, and the loop that ties them together.
    mkdir -p $out/share/cellar-kernel/cellar
    for module in ref model protocol kernel; do
      cp ../src/cellar/$module.scm $out/share/cellar-kernel/cellar/ 2>/dev/null \
        || cp src/cellar/$module.scm $out/share/cellar-kernel/cellar/
    done
    cp bin/cellar-kernel.scm $out/share/cellar-kernel/

    # Compiled ahead of time: the shell starts this on every launch, and a
    # kernel that has to compile itself first is a second of somebody waiting
    # for their sheet to appear.
    GUILE_AUTO_COMPILE=0 guild compile -L $out/share/cellar-kernel \
      -o $out/share/cellar-kernel/cellar/ref.go \
      $out/share/cellar-kernel/cellar/ref.scm
    GUILE_AUTO_COMPILE=0 guild compile -L $out/share/cellar-kernel \
      -o $out/share/cellar-kernel/cellar/model.go \
      $out/share/cellar-kernel/cellar/model.scm
    GUILE_AUTO_COMPILE=0 guild compile -L $out/share/cellar-kernel \
      -o $out/share/cellar-kernel/cellar/protocol.go \
      $out/share/cellar-kernel/cellar/protocol.scm
    GUILE_AUTO_COMPILE=0 guild compile -L $out/share/cellar-kernel \
      -o $out/share/cellar-kernel/cellar/kernel.go \
      $out/share/cellar-kernel/cellar/kernel.scm

    makeWrapper ${guile}/bin/guile $out/bin/cellar-kernel \
      --add-flags "-L" --add-flags "$out/share/cellar-kernel" \
      --add-flags "-C" --add-flags "$out/share/cellar-kernel" \
      --add-flags "-s" --add-flags "$out/share/cellar-kernel/cellar-kernel.scm" \
      --set GUILE_AUTO_COMPILE 0
    runHook postInstall
  '';

  meta.description = "The evaluator behind Cellar: sheets of Guile expressions";
}
