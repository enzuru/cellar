{ lib, stdenv, makeWrapper, guile, g-golf, blueprint-compiler
, desktop-file-utils
, glib, gtk4, libadwaita, gtksourceview5, pango, gdk-pixbuf, graphene
, harfbuzz, cairo
, gobject-introspection, adwaita-icon-theme, hicolor-icon-theme
, gsettings-desktop-schemas }:

let
  runtime = [ glib gtk4 libadwaita gtksourceview5 pango gdk-pixbuf graphene
              harfbuzz cairo gobject-introspection g-golf ];
  guileVersion = lib.versions.majorMinor guile.version;
  # Typelibs live in each package's "out" output, but several of these packages
  # (glib, pango, gdk-pixbuf) default to "bin", where there is no
  # girepository-1.0 directory at all.
  typelibPath = lib.makeSearchPath "lib/girepository-1.0"
    (map (p: lib.getOutput "out" p) runtime);
in
stdenv.mkDerivation {
  pname = "cellar";
  version = "0.1.0";

  src = ../.;

  nativeBuildInputs = [ makeWrapper blueprint-compiler desktop-file-utils ];
  buildInputs = [ guile g-golf ] ++ runtime;

  # blueprint-compiler needs GtkSource on its typelib path to resolve `using GtkSource 5;`
  GI_TYPELIB_PATH = typelibPath;

  buildPhase = ''
    runHook preBuild
    make ui
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/cellar $out/bin
    cp -r src ui bin $out/share/cellar/

    install -Dm644 data/dev.enzuru.Cellar.desktop \
      $out/share/applications/dev.enzuru.Cellar.desktop
    # The icon file name must match the application id exactly, or the desktop
    # never finds the window's icon.
    cp -r data/icons $out/share/icons
    desktop-file-validate $out/share/applications/dev.enzuru.Cellar.desktop

    # Every --add-flags value must be a single whitespace-free token:
    # makeWrapper splits them, which is why this uses a launcher script rather
    # than -e "(@ (cellar main) main)".
    makeWrapper ${guile}/bin/guile $out/bin/cellar \
      --add-flags "-L" --add-flags "$out/share/cellar/src" \
      --add-flags "-s" --add-flags "$out/share/cellar/bin/cellar.scm" \
      --set GUILE_LOAD_PATH "$out/share/cellar/src:${g-golf}/share/guile/site/${guileVersion}" \
      --set GUILE_LOAD_COMPILED_PATH "${g-golf}/lib/guile/${guileVersion}/site-ccache" \
      --set GUILE_AUTO_COMPILE 0 \
      --set CELLAR_UI_DIR "$out/share/cellar/ui" \
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

  meta.description = "A spreadsheet whose formulas are Guile expressions";
}
