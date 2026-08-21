{ lib, stdenv, fetchurl, pkg-config, texinfo, autoconf, automake, libtool
, guile, glib, gobject-introspection, libffi, makeWrapper }:

stdenv.mkDerivation rec {
  pname = "guile-g-golf";
  version = "0.8.7";

  src = fetchurl {
    url = "https://ftp.gnu.org/gnu/g-golf/g-golf-${version}.tar.gz";
    hash = "sha256-shv8mSLGQdclu4KZoACiOo41U3sc7kN+dKQF3HZJNII=";
  };

  strictDeps = false;

  nativeBuildInputs = [ pkg-config texinfo autoconf automake libtool makeWrapper guile ];
  buildInputs = [ guile glib gobject-introspection libffi ];
  propagatedBuildInputs = [ guile glib gobject-introspection libffi ];

  # Upstream's --with-guile-site=yes would try to write into Guile's own
  # (read-only) store path. Override am/guile.mk's moddir/godir instead, so the
  # modules land in our $out under the standard layout that Guile's setup-hook
  # already knows how to put on GUILE_LOAD_PATH / GUILE_LOAD_COMPILED_PATH.
  installFlags = [
    "moddir=${placeholder "out"}/share/guile/site/3.0"
    "godir=${placeholder "out"}/lib/guile/3.0/site-ccache"
  ];

  # g-golf/init.scm dlopens bare sonames (libgirepository-1.0, libglib-2.0,
  # libgobject-2.0, libg-golf). There is no /usr/lib on NixOS, so pin the three
  # system libraries to absolute store paths. libg-golf itself has to stay
  # late-bound: during the build it lives in ./libg-golf/.libs, afterwards in
  # $out/lib, so the choice is made at load time.
  postPatch = ''
    substituteInPlace g-golf/init.scm \
      --replace-fail '(dynamic-link "libgirepository-1.0")' \
                     '(dynamic-link "${gobject-introspection}/lib/libgirepository-1.0")' \
      --replace-fail '(dynamic-link "libglib-2.0")' \
                     '(dynamic-link "${glib.out}/lib/libglib-2.0")' \
      --replace-fail '(dynamic-link "libgobject-2.0")' \
                     '(dynamic-link "${glib.out}/lib/libgobject-2.0")' \
      --replace-fail '(dynamic-link "libg-golf")' \
                     "(dynamic-link (if (file-exists? \"$out/lib/libg-golf.so\") \"$out/lib/libg-golf\" \"libg-golf\"))"
  '';

  preBuild = ''
    export LD_LIBRARY_PATH="$PWD/libg-golf/.libs''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
  '';

  enableParallelBuilding = true;

  meta = with lib; {
    description = "GNU Guile Object Library for GNOME - GObject Introspection bindings";
    homepage = "https://www.gnu.org/software/g-golf/";
    license = licenses.lgpl3Plus;
    platforms = platforms.linux;
  };
}
