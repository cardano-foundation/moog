{
  pkgs,
  project,
  version,
  rewrite-libs,
  mkDarwinHomebrewBundle,
  ...
}:

let
  releaseExecutables = project.releaseExecutables.native;
  executableNames = project.releaseExecutables.names;
  moog = releaseExecutables.moog;
  packageVersion = moog.version or version;
  devArtifactVersion = "${packageVersion}-${version}";
  darwinFormulaTest = pkgs.lib.concatMapStringsSep "\n" (name: ''
    system "#{bin}/${name}", "--help"
  '') executableNames;
  smokeCommands = map (name: "${name} --help >/dev/null") executableNames;
  copyExecutables = pkgs.lib.concatMapStringsSep "\n" (name: ''
    cp ${releaseExecutables.${name}}/bin/${name} $out/unpacked
  '') executableNames;
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
        executables = releaseExecutables;
        inherit executableNames;
        formulaTest = darwinFormulaTest;
        inherit smokeCommands;
      }
      // args
    );
  tarball-derivation = pkgs.stdenv.mkDerivation {
    pname = "moog";
    inherit version;
    passthru = {
      inherit executableNames;
    };
    buildInputs = with pkgs.buildPackages; [ nix ];
    phases = [
      "unpackPhase"
      "installPhase"
    ];
    unpackPhase = ''
      mkdir -p $out/unpacked
      ${copyExecutables}
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
