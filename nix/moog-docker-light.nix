{ pkgs, project, version, }:
pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/moog/moog-light";
  tag = version;
  config = { EntryPoint = [ "moog" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [ project.packages.moog pkgs.coreutils pkgs.bash ];
  };
}
