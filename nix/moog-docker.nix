{ pkgs, project, version, }:
pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/moog/moog";
  tag = version;
  config = { EntryPoint = [ "moog" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths =
      [ pkgs.docker project.packages.moog pkgs.curl pkgs.coreutils pkgs.bash ];
  };
}
