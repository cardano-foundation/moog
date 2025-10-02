{ pkgs, linux-package, version, }:
let
  unpack = ''
    mkdir -p $out
    tar -xvf \
      ${linux-package}/anti-${version}-linux64.tar.gz \
      -C $out
  '';
in pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/antithesis/anti-oracle";
  tag = version;
  config = { EntryPoint = [ "./anti-oracle" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = let
      unpacked = pkgs.runCommand "unpack" {
        nativeBuildInputs = [ pkgs.gnutar pkgs.gzip ];
      } unpack;
    in [ unpacked pkgs.cacert ];
  };
}
