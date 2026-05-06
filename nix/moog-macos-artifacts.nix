{
  pkgs,
  project,
  version,
  rewrite-libs,
  mkDarwinHomebrewBundle,
  ...
}:

let
  moog = project.packages.moog;
  moog-oracle = project.packages.moog-oracle;
  packageVersion = moog.version or version;
  devArtifactVersion = "${packageVersion}-${version}";
  darwinExecutables = {
    inherit moog moog-oracle;
  };
  darwinExecutableNames = [
    "moog"
    "moog-oracle"
  ];
  darwinFormulaTest = ''
    system "#{bin}/moog", "--help"
    system "#{bin}/moog-oracle", "--help"
  '';
  mkMoogDarwinHomebrewBundle =
    args:
    mkDarwinHomebrewBundle (
      {
        pname = "moog";
        version = packageVersion;
        owner = "cardano-foundation";
        desc = "Administer Antithesis test execution for Cardano";
        homepage = "https://github.com/cardano-foundation/moog";
        formulaClass = "Moog";
        executables = darwinExecutables;
        executableNames = darwinExecutableNames;
        formulaTest = darwinFormulaTest;
        smokeCommands = [
          "moog --help >/dev/null"
          "moog-oracle --help >/dev/null"
        ];
      }
      // args
    );
  tarball-derivation = pkgs.stdenv.mkDerivation {
    pname = "moog";
    inherit version;
    buildInputs = with pkgs.buildPackages; [ nix ];
    phases = [
      "unpackPhase"
      "installPhase"
    ];
    unpackPhase = ''
      mkdir -p $out/unpacked
      cp ${moog}/bin/moog $out/unpacked
      cp ${moog-oracle}/bin/moog-oracle $out/unpacked
      ( cd $out/unpacked ;
        ${rewrite-libs}/bin/rewrite-libs . `ls -1 | grep -Fv .dylib`
        for a in *; do /usr/bin/codesign -f -s - $a; done
      )
      chmod -R +w $out/unpacked/*
    '';
    installPhase = ''
      tar -C $out/unpacked -czvf $out/$pname-$version-darwin64.tar.gz .
      rm -rf $out/unpacked
    '';
  };

in
{
  packages = {
    darwin64.tarball = tarball-derivation;
    darwin-release-artifacts = mkMoogDarwinHomebrewBundle { };
    darwin-dev-homebrew-artifacts = mkMoogDarwinHomebrewBundle {
      artifactVersion = devArtifactVersion;
      releaseTag = "dev-homebrew";
      formulaName = "moog-dev";
      formulaClass = "MoogDev";
      formulaVersion = devArtifactVersion;
      formulaExtraLines = "\n  conflicts_with \"moog\", because: \"both install the same command-line tools\"";
    };
  };
}
