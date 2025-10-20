{ project, pkgs, version, }:
pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/antithesis/anti-oracle";
  tag = version;
  config = { EntryPoint = [ "anti-oracle" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [ project.packages.anti-oracle pkgs.cacert pkgs.docker ];
  };
}
