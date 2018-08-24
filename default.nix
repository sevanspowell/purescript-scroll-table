let
  pkgs = import ./nix/nixpkgs-pinned {};
in
  pkgs.stdenv.mkDerivation {
    name = "scroll-table";

    src = ./.;

    buildInputs = [
      pkgs.nodejs
      pkgs.nodePackages.pulp
      pkgs.psc-package
      pkgs.purescript
    ];
  }
