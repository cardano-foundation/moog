{ CHaP, indexState, pkgs, cardano-cli, ... }:

let
  shell = { pkgs, ... }: {
    tools = {
      cabal = { index-state = indexState; };
      cabal-fmt = { index-state = indexState; };
      haskell-language-server = { index-state = indexState; };
      hoogle = { index-state = indexState; };
      fourmolu = { index-state = indexState; };
      hlint = { index-state = indexState; };
    };
    withHoogle = true;
    buildInputs = [
      pkgs.gitAndTools.git
      pkgs.just
      pkgs.nixfmt-classic

    ];
    shellHook = ''
      echo "Entering shell for adversary development"
    '';
  };
  fix-libs = ({ lib, pkgs, ... }: {
    # Use the VRF fork of libsodium
    packages.cardano-crypto-praos.components.library.pkgconfig =
      lib.mkForce [ [ pkgs.libsodium-vrf ] ];
    packages.cardano-crypto-class.components.library.pkgconfig =
      lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 pkgs.libblst ]];
  });
  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "adversary";
    src = ./..;
    compiler-nix-name = "ghc984";
    shell = shell { inherit pkgs; };

    modules = [ fix-libs ];
    inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
  };
  project = pkgs.haskell-nix.cabalProject' mkProject;

in {
  devShells.default = project.shell;
  packages.adversary = project.hsPkgs.adversary.components.exes.adversary;
  inherit project;
}
