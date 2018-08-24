{ config ? {} }:

let
  fetch-nixpkgs = import ./fetch-nixpkgs.nix;

  nixpkgs-args = builtins.fromJSON (builtins.readFile ./nixpkgs-2.0.json);

  nixpkgs = fetch-nixpkgs {
    inherit (nixpkgs-args) owner repo rev sha256;
  };

  purescriptOverlay = (self: super: {
      purescript = super.callPackage ../pkgs/purescript {};
  });

  pkgs = import nixpkgs {
    inherit config;
    overlays = [purescriptOverlay];
  };

in
  pkgs
