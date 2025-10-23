{ pkgs, project, node-project, version, rewrite-libs, ... }:

let
  inherit (pkgs) lib;
  moog = project.packages.moog;
  moog-oracle = project.packages.moog-oracle;
  tarball-derivation = pkgs.stdenv.mkDerivation {
    pname = "moog";
    inherit version;
    buildInputs = with pkgs.buildPackages; [ nix ];
    phases = [ "unpackPhase" "installPhase" ];
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

in { packages.darwin64.tarball = tarball-derivation; }
