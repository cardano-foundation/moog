{ project, pkgs, version, }:
pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/moog/moog-oracle";
  tag = version;
  architecture = if pkgs.stdenv.hostPlatform.isAarch64 then "arm64" else "amd64";
  config = { EntryPoint = [ "moog-oracle" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [ project.packages.moog-oracle pkgs.cacert pkgs.docker ];
  };
}
