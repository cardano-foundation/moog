{
  description = "Antithesis CLI";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    bundlers = {
      url = "github:NixOS/bundlers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dev-assets.url = "github:paolino/dev-assets/v0.1.0";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages/repo";
      flake = false;
    };
    cardano-mpfs-offchain = {
      url =
        "github:lambdasistemi/cardano-mpfs-offchain/c33c1bdd658b725965c9f404ec0477dabea70de3";
      flake = false;
    };
    cardano-node-runtime = {
      url = "github:IntersectMBO/cardano-node?ref=10.7.0";
    };
    mkdocs.url = "github:paolino/dev-assets?dir=mkdocs";
    asciinema.url = "github:paolino/dev-assets?dir=asciinema";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-utils,
      haskellNix,
      CHaP,
      iohkNix,
      cardano-node-runtime,
      mkdocs,
      asciinema,
      ...
    }:
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

      # The Haskell `lzma` package declares `extra-libraries: lzma`, which
      # haskell.nix resolves to the top-level `pkgs.lzma` attribute. Recent
      # nixpkgs removed that attribute (it became `pkgs.xz`, which still
      # provides liblzma), so the lookup threw "Nixpkgs package set does not
      # contain the package: lzma" and broke `nix develop`. Restore the
      # historical alias so the system-library mapping resolves again.
      fix-lzma = final: prev: { lzma = prev.xz; };

      perSystem =
        system:
        let
          isLinux = builtins.match ".*-linux" system != null;
          isDarwin = builtins.match ".*-darwin" system != null;

          node-project =
            if builtins.hasAttr system (cardano-node-runtime.project or { }) then
              cardano-node-runtime.project.${system}
            else
              null;
          cardano-cli = if node-project != null then node-project.pkgs.cardano-cli else null;
          pkgs = import nixpkgs {
            overlays = [
              iohkNix.overlays.crypto # modified crypto libs
              iohkNix.overlays.haskell-nix-crypto
              iohkNix.overlays.cardano-lib
              haskellNix.overlay # some functions
              fix-blst
              fix-lzma
            ];
            inherit system;
          };
          project = import ./nix/moog-project.nix {
            indexState = "2026-02-17T10:15:41Z";
            inherit CHaP;
            cardano-mpfs-offchain = inputs.cardano-mpfs-offchain;
            inherit pkgs;
            inherit cardano-cli;
            mkdocs = mkdocs.packages.${system};
            asciinema = asciinema.packages.${system};
          };

          packageVersion =
            let
              versionLines = builtins.filter (lib.hasPrefix "version:") (
                lib.splitString "\n" (builtins.readFile ./moog.cabal)
              );
            in
            builtins.elemAt (builtins.match "version:[[:space:]]*([^[:space:]]+)" (builtins.head versionLines)) 0;
          sourceRevision = self.shortRev or (self.dirtyShortRev or "dirty");
          devArtifactVersion = "${packageVersion}-${sourceRevision}";
          mkDarwinHomebrewBundle = inputs.dev-assets.lib.mkDarwinHomebrewBundle { inherit pkgs; };
          exeSpecs = [
            {
              name = "moog";
              package = project.packages.moog;
              desc = "CLI to administer Antithesis test execution through Cardano";
              formulaClass = "Moog";
            }
            {
              name = "moog-oracle";
              package = project.packages.moog-oracle;
              desc = "Moog oracle service for Antithesis test validation";
              formulaClass = "MoogOracle";
            }
            {
              name = "moog-agent";
              package = project.packages.moog-agent;
              desc = "Moog agent service for Antithesis result publication";
              formulaClass = "MoogAgent";
            }
          ];
          muslComponents =
            if system == "x86_64-linux" then
              project.musl64.moog.components
            else if system == "aarch64-linux" then
              project.aarch64-musl.moog.components
            else
              null;
          mkExeSmokeCommand = spec: ''
            ${spec.name} --help >/tmp/${spec.name}.out 2>&1
            grep -F -- "Usage:" /tmp/${spec.name}.out >/dev/null
          '';
          mkExeLinuxRelease =
            spec: extraArgs:
            inputs.dev-assets.lib.mkLinuxArtifacts (
              {
                inherit pkgs system;
                executableName = spec.name;
                version = packageVersion;
                glibcPackage = spec.package;
                muslPackage = muslComponents.exes.${spec.name};
                bundlers = inputs.bundlers;
              }
              // extraArgs
            );
          mkExeDarwinHomebrewBundle =
            spec: args:
            mkDarwinHomebrewBundle (
              {
                pname = spec.name;
                version = packageVersion;
                owner = "cardano-foundation";
                repo = "moog";
                desc = spec.desc;
                formulaClass = spec.formulaClass;
                executables = {
                  ${spec.name} = spec.package;
                };
                executableNames = [ spec.name ];
                formulaTest = ''
                  output = shell_output("#{bin}/${spec.name} --help")
                  assert_match "Usage:", output
                '';
                smokeCommands = [ (mkExeSmokeCommand spec) ];
              }
              // args
            );
          linuxReleasePackages = lib.optionalAttrs pkgs.stdenv.isLinux (
            (lib.listToAttrs (
              lib.concatMap (spec: [
                {
                  name = "${spec.name}-linux-release-artifacts";
                  value = mkExeLinuxRelease spec { };
                }
                {
                  name = "${spec.name}-linux-dev-release-artifacts";
                  value = mkExeLinuxRelease spec {
                    artifactVersion = devArtifactVersion;
                  };
                }
              ]) exeSpecs
            ))
            // {
              linux-artifact-smoke = inputs.dev-assets.lib.mkLinuxArtifactSmoke {
                inherit pkgs system;
              };
            }
          );
          darwinReleasePackages = lib.optionalAttrs pkgs.stdenv.isDarwin (
            lib.listToAttrs (
              lib.concatMap (spec: [
                {
                  name = "${spec.name}-darwin-release-artifacts";
                  value = mkExeDarwinHomebrewBundle spec { };
                }
                {
                  name = "${spec.name}-darwin-dev-homebrew-artifacts";
                  value = mkExeDarwinHomebrewBundle spec {
                    artifactVersion = devArtifactVersion;
                    releaseTag = "dev-homebrew-${spec.name}";
                    formulaName = "${spec.name}-dev";
                    formulaClass = "${spec.formulaClass}Dev";
                    formulaVersion = devArtifactVersion;
                  };
                }
              ]) exeSpecs
            )
          );

          docker =
            if isLinux then
              {
                packages = {
                  moog-oracle-docker-image = import ./nix/moog-oracle-docker.nix {
                    inherit pkgs version project;
                  };
                  moog-agent-docker-image = import ./nix/moog-agent-docker.nix {
                    inherit pkgs project version;
                  };
                  moog-antithesis-proxy-docker-image = import ./nix/moog-antithesis-proxy-docker.nix {
                    inherit pkgs project version;
                  };
                  moog-docker-image = import ./nix/moog-docker.nix {
                    inherit pkgs version project;
                  };
                  moog-docker-light-image = import ./nix/moog-docker-light.nix {
                    inherit pkgs version project;
                  };
                };
              }
            else
              { packages = { }; };

          info.packages = {
            version = packageVersion;
          };
          fullPackages = lib.mergeAttrsList [
            project.packages
            linuxReleasePackages
            darwinReleasePackages
            info.packages
            docker.packages
          ];

        in
        {

          packages = fullPackages // {
            default = project.packages.moog;
          };
          inherit (project) devShells;
        };

    in
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ] perSystem;
}
