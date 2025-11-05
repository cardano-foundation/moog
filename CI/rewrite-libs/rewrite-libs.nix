{ system, nixpkgs, haskellNix, flake-utils, ... }:
let
  src = ./.;
  indexState = "2025-05-07T00:00:00Z";
  pkgs = import nixpkgs {
    overlays = [ haskellNix.overlay ];
    inherit system;
  };
in import ./nix/project.nix {
  inherit system;
  inherit indexState;
  inherit src;
  inherit (pkgs) haskell-nix;
  inherit pkgs;
}
