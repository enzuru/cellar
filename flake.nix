{
  description = "Cellar - a spreadsheet whose formulas are Guile code (GTK4 + libadwaita + G-Golf)";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAll = f: nixpkgs.lib.genAttrs systems (s: f nixpkgs.legacyPackages.${s});
    in {
      packages = forAll (pkgs: rec {
        g-golf = pkgs.callPackage ./nix/g-golf.nix { };
        cellar = pkgs.callPackage ./nix/cellar.nix { inherit g-golf; };
        default = cellar;
      });

      devShells = forAll (pkgs:
        let
          g-golf = pkgs.callPackage ./nix/g-golf.nix { };
          runtime = with pkgs; [
            glib gtk4 libadwaita gtksourceview5 pango gdk-pixbuf graphene
            harfbuzz cairo gobject-introspection g-golf
          ];
        in {
          default = pkgs.mkShell {
            packages = with pkgs; [
              guile_3_0 g-golf blueprint-compiler gnumake pkg-config
              gtk4 libadwaita gtksourceview5 gobject-introspection
              gtk4.dev adwaita-icon-theme
            ];

            # libgirepository dlopens the bare sonames recorded in each .typelib
            # (libgtk-4.so.1, ...), and there is no /usr/lib on NixOS, so the
            # loader needs to be told where they live.
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath runtime;
            # "out", not the default output: glib and pango default to "bin",
            # which has no girepository-1.0 directory.
            GI_TYPELIB_PATH = pkgs.lib.makeSearchPath "lib/girepository-1.0"
              (map (p: pkgs.lib.getOutput "out" p) runtime);

            shellHook = ''
              export XDG_DATA_DIRS="${pkgs.gtk4}/share/gsettings-schemas/${pkgs.gtk4.name}:${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.adwaita-icon-theme}/share:${pkgs.gtk4}/share:$XDG_DATA_DIRS"
              echo "cellar dev shell -- run 'make run'"
            '';
          };
        });
    };
}
