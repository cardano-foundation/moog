{
  pkgs,
  node-project,
  version,
  project,
  ...
}:
let
  releaseExecutables = project.releaseExecutables.musl64;
  executableNames = project.releaseExecutables.names;
  copyExecutables = pkgs.lib.concatMapStringsSep "\n" (name: ''
    cp ${releaseExecutables.${name}}/bin/${name} $out/unpacked
  '') executableNames;
  tarball-derivation = pkgs.stdenv.mkDerivation rec {
    pname = "moog";
    inherit version;
    passthru = {
      inherit executableNames;
    };
    unpackPhase = ''
      mkdir -p $out/unpacked
      ${copyExecutables}
      chmod -R +w $out/unpacked/*
    '';
    installPhase = ''
      tar -C $out/unpacked -czvf $out/$pname-$version-linux64.tar.gz .
      rm -rf $out/unpacked
    '';
  };
in
{
  packages.linux64.tarball = tarball-derivation;
}
