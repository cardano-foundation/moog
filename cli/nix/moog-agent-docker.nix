{ pkgs, project, version, }:
pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/moog/moog-agent";
  tag = version;
  config = { EntryPoint = [ "moog-agent" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [ pkgs.docker project.packages.moog-agent pkgs.curl pkgs.coreutils pkgs.bash ];
  };
}
