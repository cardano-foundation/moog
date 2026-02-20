{
  description = "Antithesis CLI";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
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
    mkdocs.url = "github:paolino/dev-assets?dir=mkdocs";
    asciinema.url = "github:paolino/dev-assets?dir=asciinema";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, CHaP, iohkNix
    , cardano-node-runtime, mkdocs, asciinema, ... }:
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
          isLinux = builtins.match ".*-linux" system != null;
          isDarwin = builtins.match ".*-darwin" system != null;

          node-project =
            if builtins.hasAttr system (cardano-node-runtime.project or { })
            then cardano-node-runtime.project.${system}
            else null;
          cardano-cli =
            if node-project != null then node-project.pkgs.cardano-cli
            else null;
          pkgs = import nixpkgs {
            overlays = [
              iohkNix.overlays.crypto # modified crypto libs
              iohkNix.overlays.cardano-lib
              haskellNix.overlay # some functions
              fix-blst
            ];
            inherit system;
          };
          project = import ./nix/moog-project.nix {
            indexState = "2025-08-07T00:00:00Z";
            inherit CHaP;
            inherit pkgs;
            inherit cardano-cli;
            mkdocs = mkdocs.packages.${system};
            asciinema = asciinema.packages.${system};
          };

          linux-artifacts =
            if system == "x86_64-linux"
            then import ./nix/moog-linux-artifacts.nix {
              inherit pkgs node-project version project;
            }
            else { packages = { }; };

          linux-aarch64-artifacts =
            if system == "aarch64-linux"
            then import ./nix/moog-linux-aarch64-artifacts.nix {
              inherit pkgs version project;
            }
            else { packages = { }; };

          macos-artifacts =
            if isDarwin
            then
              let
                rewrite-libs = import ./CI/rewrite-libs/rewrite-libs.nix {
                  inherit system;
                  inherit (inputs) nixpkgs flake-utils haskellNix;
                };
              in import ./nix/moog-macos-artifacts.nix {
                inherit pkgs project node-project version;
                rewrite-libs = rewrite-libs.packages.default;
              }
            else { packages = { }; };

          docker = if isLinux then {
            packages = {
              moog-oracle-docker-image = import ./nix/moog-oracle-docker.nix {
                inherit pkgs version project;
              };
              moog-agent-docker-image = import ./nix/moog-agent-docker.nix {
                inherit pkgs project version;
              };
              moog-docker-image = import ./nix/moog-docker.nix {
                inherit pkgs version project;
              };
              moog-docker-light-image = import ./nix/moog-docker-light.nix {
                inherit pkgs version project;
              };
            };
          } else { packages = { }; };

          info.packages = { inherit version; };
          fullPackages = lib.mergeAttrsList [
            project.packages
            linux-artifacts.packages
            linux-aarch64-artifacts.packages
            macos-artifacts.packages
            info.packages
            docker.packages
          ];

        in {

          packages = fullPackages // { default = project.packages.moog; };
          inherit (project) devShells;
        };

    in flake-utils.lib.eachSystem
      [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ] perSystem;
}
