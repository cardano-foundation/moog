{ pkgs, project, version, }:
pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/moog/moog-agent";
  tag = version;
  architecture = if pkgs.stdenv.hostPlatform.isAarch64 then "arm64" else "amd64";
  config = { EntryPoint = [ "moog-agent" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [
      pkgs.docker
      project.packages.moog-agent
      pkgs.curl
      pkgs.coreutils
      pkgs.bash
    ];
  };
}
