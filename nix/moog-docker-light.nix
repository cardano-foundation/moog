{ pkgs, project, version, }:
pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/moog/moog-light";
  tag = version;
  architecture = if pkgs.stdenv.hostPlatform.isAarch64 then "arm64" else "amd64";
  config = { EntryPoint = [ "moog" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [ project.packages.moog pkgs.coreutils pkgs.bash ];
  };
}
