{
  description = "Antithesis CLI";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.url = "github:NixOS/nixpkgs";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    cardano-node-runtime = {
      url = "github:IntersectMBO/cardano-node?ref=10.1.4";
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, CHaP, iohkNix
    , cardano-node-runtime, ... }:
    let
      lib = nixpkgs.lib;
      version = self.dirtyShortRev or self.shortRev;

      fix-blst = final: prev: {
        haskell-nix = prev.haskell-nix // {
          extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings // {
            # String pkgconfig-depends names are mapped to lists of Nixpkgs
            # package names
            "libblst" = [ "blst" ];
          };
        };
      };

      perSystem = system:
        let
          node-pkgs = cardano-node-runtime.project.${system}.pkgs;
          cardano-node = node-pkgs.cardano-node;
          cardano-cli = node-pkgs.cardano-cli;
          cardano-submit-api = node-pkgs.cardano-submit-api;
          pkgs = import nixpkgs {
            overlays = [
              iohkNix.overlays.crypto # modified crypto libs
              iohkNix.overlays.cardano-lib
              haskellNix.overlay # some functions
              fix-blst
            ];
            inherit system;
          };

        in import ./project.nix {
          indexState = "2025-05-07T00:00:00Z";
          inherit CHaP;
          inherit version;
          inherit pkgs;
          inherit cardano-node;
          inherit cardano-cli;
          inherit cardano-submit-api;
        };

    in flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] perSystem;
}
