{ pkgs, project, version, }:
pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/antithesis/anti-agent";
  tag = version;
  config = { EntryPoint = [ "anti-agent" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [ project.packages.anti-agent pkgs.curl pkgs.coreutils pkgs.bash ];
  };
}
