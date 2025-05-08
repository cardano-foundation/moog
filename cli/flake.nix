{
  description = "Antithesis CLI";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.url = "github:NixOS/nixpkgs";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, ... }:
    let
      lib = nixpkgs.lib;
      version = self.dirtyShortRev or self.shortRev;

      perSystem = system:
        let

          pkgs = import nixpkgs {
            overlays = [ haskellNix.overlay ];
            inherit system;
          };
          indexState = "2025-05-01T00:00:00Z";

          shell = { pkgs, ... }: {
            tools = {
              cabal = { index-state = indexState; };
              cabal-fmt = { index-state = indexState; };
              haskell-language-server = { index-state = indexState; };
              hoogle = { index-state = indexState; };
              fourmolu = { index-state = indexState; };
            };
            withHoogle = true;
            buildInputs = [
              pkgs.just
              pkgs.gitAndTools.git
              pkgs.haskellPackages.ghcid
              pkgs.haskellPackages.hlint
            ];
            shellHook = ''
              echo "Entering shell for anti CLI development"
            '';
          };
          mkProject = ctx@{ lib, pkgs, ... }: {
            name = "anti";
            src = ./.;
            compiler-nix-name = "ghc984";
            shell = shell { inherit pkgs; };
          };
          project = pkgs.haskell-nix.cabalProject' mkProject;

        in {
          devShells.default = project.shell;
          inherit project;
          inherit version;
        };
    in flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] perSystem;
}
