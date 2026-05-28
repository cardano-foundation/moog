{ CHaP, indexState, pkgs, cardano-cli ? null, mkdocs, asciinema, ... }:

let
  libOverlay = { lib, pkgs, ... }: {
    # Use our forked libsodium from iohk-nix crypto overlay.
    packages.plutus-tx.components.library.pkgconfig =
      lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 ]];
    packages.byron-spec-ledger.components.library.pkgconfig =
      lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 ]];
    packages.cardano-crypto-praos.components.library.pkgconfig =
      lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 ]];
    packages.cardano-crypto-class.components.library.pkgconfig =
      lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 pkgs.libblst ]];
    packages.cardano-ledger-binary.components.library.doHaddock =
      lib.mkForce false;
    packages.plutus-core.components.library.doHaddock =
      lib.mkForce false;
    packages.plutus-ledger-api.components.library.doHaddock =
      lib.mkForce false;
    packages.plutus-tx.components.library.doHaddock =
      lib.mkForce false;
  };

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
    buildInputs = [ pkgs.git pkgs.just ]
      ++ pkgs.lib.optional (cardano-cli != null) cardano-cli ++ [
        project.hsPkgs.cardano-addresses.components.exes.cardano-address
        project.hsPkgs.bech32.components.exes.bech32
        pkgs.nixfmt-classic
        pkgs.mkdocs
        mkdocs.from-nixpkgs
        mkdocs.asciinema-plugin
        mkdocs.markdown-callouts
        mkdocs.markdown-graphviz
        asciinema.compress
        asciinema.resize
        pkgs.asciinema
      ];
    shellHook = ''
      echo "Entering shell for moog CLI development"
    '';
  };

  fullyStaticOptions = { pkgs, ... }:
    let libs = with pkgs; [ zlib openssl libffi gmp6 pkgs.secp256k1 ];
    in {
      enableShared = false;
      enableStatic = true;
      configureFlags = map (l: "--ghc-option=-optl=-L${l}/lib") (libs);
    };
  musl = { pkgs, ... }: {
    packages.moog.components.exes.moog = (fullyStaticOptions { inherit pkgs; });
    doHaddock = false;
  };
  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "moog";
    src = ./..;
    compiler-nix-name = "ghc9123";
    shell = shell { inherit pkgs; };
    modules = [ libOverlay ];
    inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
  };
  project = pkgs.haskell-nix.cabalProject' mkProject;

in {
  devShells.default = project.shell;
  inherit project;
  packages.moog = project.hsPkgs.moog.components.exes.moog;
  packages.moog-oracle = project.hsPkgs.moog.components.exes.moog-oracle;
  packages.moog-agent = project.hsPkgs.moog.components.exes.moog-agent;
  packages.moog-mpfs-v2-canary =
    project.hsPkgs.moog.components.exes.moog-mpfs-v2-canary;
  packages.moog-github-device-flow-smoke =
    project.hsPkgs.moog.components.exes.moog-github-device-flow-smoke;
  packages.moog-github-auth-smoke =
    project.hsPkgs.moog.components.exes.moog-github-auth-smoke;
  packages.bech32 = project.hsPkgs.bech32.components.exes.bech32;
  packages.cardano-address =
    project.hsPkgs.cardano-addresses.components.exes.cardano-address;
  packages.unit-tests = project.hsPkgs.moog.components.tests.unit-tests;
  packages.integration-tests =
    project.hsPkgs.moog.components.tests.integration-tests;
  packages.e2e-tests = project.hsPkgs.moog.components.tests.e2e-tests;
  musl64 = project.projectCross.musl64.hsPkgs;
  aarch64-musl = project.projectCross.aarch64-multiplatform-musl.hsPkgs;
}
