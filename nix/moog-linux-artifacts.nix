{ pkgs, node-project, version, project, ... }:
let
  moog = project.musl64.moog.components.exes.moog;
  moog-oracle = project.musl64.moog.components.exes.moog-oracle;
  moog-agent = project.musl64.moog.components.exes.moog-agent;
  tarball-derivation = pkgs.stdenv.mkDerivation rec {
    pname = "moog";
    inherit version;
    unpackPhase = ''
      mkdir -p $out/unpacked
      cp ${moog}/bin/moog $out/unpacked
      cp ${moog-oracle}/bin/moog-oracle $out/unpacked
      cp ${moog-agent}/bin/moog-agent $out/unpacked
      chmod -R +w $out/unpacked/*
    '';
    installPhase = ''
      tar -C $out/unpacked -czvf $out/$pname-$version-linux64.tar.gz .
      rm -rf $out/unpacked
    '';
  };
in { packages.linux64.tarball = tarball-derivation; }
