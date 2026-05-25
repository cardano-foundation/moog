{
  system,
  nixpkgs,
  haskellNix,
  flake-utils,
  ...
}:
let
  src = ./.;
  indexState = "2026-02-17T10:15:41Z";
  pkgs = import nixpkgs {
    overlays = [ haskellNix.overlay ];
    inherit system;
  };
in
import ./nix/project.nix {
  inherit system;
  inherit indexState;
  inherit src;
  inherit (pkgs) haskell-nix;
  inherit pkgs;
}
