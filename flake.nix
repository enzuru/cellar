{
  description = "Cellar - a spreadsheet whose cells are Guile code (Haskell shell + Guile kernel, GTK4 + libadwaita)";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAll = f: nixpkgs.lib.genAttrs systems (s: f nixpkgs.legacyPackages.${s});

      # The libraries both halves need at runtime.  The Haskell shell links
      # against them; the Guile kernel needs none of them, but it is started by
      # the shell and inherits its environment, so they are listed once.
      runtimeLibs = pkgs: with pkgs; [
        glib gtk4 libadwaita gtksourceview5 pango gdk-pixbuf graphene
        harfbuzz cairo gobject-introspection
      ];

      # haskell-gi generates each binding from the .gir files at build time, so
      # the introspection data has to be there to build against and not only to
      # run against.
      # gi-gtk4 and gi-gdk4 rather than the gi-gtk / gi-gdk shims: gi-adwaita is
      # built against those, and a widget from one is not a widget from the
      # other as far as the type checker is concerned.  Naming the shims here
      # as well would put two modules called GI.Gtk on the search path and make
      # every import ambiguous.
      haskellDeps = ps: with ps; [
        base bytestring containers directory filepath process text unix
        haskell-gi-base gi-glib gi-gobject gi-gio gi-gdk4 gi-graphene gi-gtk4
        gi-adwaita
        gi-gtksource5 gi-pango
      ] ++ declarativeDeps ps;

      # gi-gtk4-declarative is not published anywhere yet, so the shell
      # compiles it from the checkout next door rather than depending on a
      # package.  What it needs has to be here, since the compiler call that
      # builds Cellar is the one that builds it.
      #
      # The library's cabal files name gi-gtk and gi-gdk, which are shims over
      # gi-gtk4 and gi-gdk4 and hold the same types.  They are deliberately
      # absent here: two exposed packages with a module called GI.Gtk make
      # every import of it ambiguous, and a direct compiler call names no
      # packages.  The library's `import qualified GI.Gtk' finds gi-gtk4.
      declarativeDeps = ps: with ps; [
        # The library itself.
        data-default-class mtl unordered-containers vector
        haskell-gi haskell-gi-overloading
        # app-simple, whose inputs are pipes Producers.
        async pipes pipes-concurrency
      ];
    in {
      packages = forAll (pkgs: rec {
        # The kernel: Guile, and nothing to do with GTK.
        cellar-kernel = pkgs.callPackage ./nix/kernel.nix { };
        # The shell: Haskell, and everything to do with GTK.
        cellar = pkgs.callPackage ./nix/cellar.nix { inherit cellar-kernel; };
        default = cellar;
      });

      devShells = forAll (pkgs:
        let
          ghc = pkgs.haskellPackages.ghcWithPackages haskellDeps;
          runtime = runtimeLibs pkgs;
        in {
          default = pkgs.mkShell {
            packages = with pkgs; [
              # The shell half.
              ghc
              cabal-install
              haskellPackages.haskell-language-server
              # The kernel half.
              guile_3_0
              # Both.
              blueprint-compiler gnumake pkg-config
              gtk4 gtk4.dev libadwaita gtksourceview5 gobject-introspection
              adwaita-icon-theme hicolor-icon-theme
            ];

            # haskell-gi's generated code dlopens the bare sonames recorded in
            # each .typelib (libgtk-4.so.1, ...), and there is no /usr/lib on
            # NixOS, so the loader needs to be told where they live.
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath runtime;
            # "out", not the default output: glib and pango default to "bin",
            # which has no girepository-1.0 directory.
            GI_TYPELIB_PATH = pkgs.lib.makeSearchPath "lib/girepository-1.0"
              (map (p: pkgs.lib.getOutput "out" p) runtime);

            shellHook = ''
              export XDG_DATA_DIRS="${pkgs.gtk4}/share/gsettings-schemas/${pkgs.gtk4.name}:${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.adwaita-icon-theme}/share:${pkgs.hicolor-icon-theme}/share:${pkgs.gtk4}/share:$XDG_DATA_DIRS"
              echo "cellar dev shell -- run 'make run'"
            '';
          };
        });
    };
}
