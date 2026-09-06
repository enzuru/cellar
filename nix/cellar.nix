{ lib, stdenv, haskellPackages, haskell, makeWrapper, blueprint-compiler
, desktop-file-utils, cellar-kernel
, glib, gtk4, libadwaita, gtksourceview5, pango, gdk-pixbuf, graphene
, harfbuzz, cairo
, gobject-introspection, adwaita-icon-theme, hicolor-icon-theme
, gsettings-desktop-schemas }:

# The shell half of Cellar: the window, the folder on disk, the tabs.
#
# It is a Haskell program that starts `cellar-kernel`, a Guile program, and
# talks to it over a pipe.  The two are packaged separately because they have
# nothing in common but the wire format -- the kernel needs no GTK and the
# shell needs no evaluator.

let
  runtime = [ glib gtk4 libadwaita gtksourceview5 pango gdk-pixbuf graphene
              harfbuzz cairo gobject-introspection ];

  # Typelibs live in each package's "out" output, but several of these packages
  # (glib, pango, gdk-pixbuf) default to "bin", where there is no
  # girepository-1.0 directory at all.
  typelibPath = lib.makeSearchPath "lib/girepository-1.0"
    (map (p: lib.getOutput "out" p) runtime);

  # The test suite drives a real Guile kernel over a real pipe, which is the
  # right thing for `make check` and the wrong thing for a sandboxed build.
  shell = haskell.lib.compose.dontCheck
    (haskellPackages.callCabal2nix "cellar" ../. { });

  # The .ui files, compiled from Blueprint, and the stylesheet that goes with
  # them.  A separate derivation so that the Haskell build does not have to
  # know what Blueprint is.
  ui = stdenv.mkDerivation {
    pname = "cellar-ui";
    version = "0.1.0";
    src = ../ui;
    nativeBuildInputs = [ blueprint-compiler ];
    GI_TYPELIB_PATH = typelibPath;
    buildPhase = ''
      runHook preBuild
      for blueprint in *.blp; do
        blueprint-compiler compile --output "''${blueprint%.blp}.ui" "$blueprint"
      done
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      mkdir -p $out
      cp *.ui *.css $out/
      runHook postInstall
    '';
  };
in
stdenv.mkDerivation {
  pname = "cellar";
  version = "0.1.0";

  src = ../.;

  nativeBuildInputs = [ makeWrapper desktop-file-utils ];

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/cellar $out/bin
    cp -r ${ui} $out/share/cellar/ui

    install -Dm644 data/dev.enzuru.Cellar.desktop \
      $out/share/applications/dev.enzuru.Cellar.desktop
    # The icon file name must match the application id exactly, or the desktop
    # never finds the window's icon.
    cp -r data/icons $out/share/icons
    desktop-file-validate $out/share/applications/dev.enzuru.Cellar.desktop

    makeWrapper ${shell}/bin/cellar $out/bin/cellar \
      --set CELLAR_UI_DIR "$out/share/cellar/ui" \
      `# The kernel is a program of its own; the shell is told where it is` \
      `# rather than going looking, since an installed Cellar has no source` \
      `# tree to look in.` \
      --set CELLAR_KERNEL "${cellar-kernel}/bin/cellar-kernel" \
      `# Set, not prefix: an inherited GI_TYPELIB_PATH from the host can point` \
      `# at a different glib, and mixing typelibs across glib versions trips` \
      `# g_binding_class_init's assertion at startup.` \
      --set GI_TYPELIB_PATH "${typelibPath}" \
      --set LD_LIBRARY_PATH "${lib.makeLibraryPath runtime}" \
      `# $out/share carries Cellar's own icons; hicolor supplies the index.theme` \
      `# that makes scalable/apps and symbolic/apps searchable at all.` \
      --prefix XDG_DATA_DIRS : "$out/share:${hicolor-icon-theme}/share:${adwaita-icon-theme}/share:${gtk4}/share/gsettings-schemas/${gtk4.name}:${gsettings-desktop-schemas}/share/gsettings-schemas/${gsettings-desktop-schemas.name}"
    runHook postInstall
  '';

  meta.description = "A spreadsheet whose cells are Guile expressions";
}
