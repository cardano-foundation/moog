{ pkgs, project, version, }:
pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/moog/moog";
  tag = version;
  architecture = if pkgs.stdenv.hostPlatform.isAarch64 then "arm64" else "amd64";
  config = { EntryPoint = [ "moog" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths =
      [ pkgs.docker project.packages.moog pkgs.curl pkgs.coreutils pkgs.bash ];
  };
}
