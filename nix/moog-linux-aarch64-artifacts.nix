{ pkgs, version, project, ... }:
let
  moog = project.aarch64-musl.moog.components.exes.moog;
  moog-oracle = project.aarch64-musl.moog.components.exes.moog-oracle;
  moog-agent = project.aarch64-musl.moog.components.exes.moog-agent;
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
      tar -C $out/unpacked -czvf $out/$pname-$version-linux-aarch64.tar.gz .
      rm -rf $out/unpacked
    '';
  };
in { packages.linux-aarch64.tarball = tarball-derivation; }
