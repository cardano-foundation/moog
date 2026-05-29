{ pkgs, project, version, }:
pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/moog/moog-antithesis-proxy";
  tag = version;
  architecture = if pkgs.stdenv.hostPlatform.isAarch64 then "arm64" else "amd64";
  config = {
    EntryPoint = [ "moog-antithesis-proxy" ];
    ExposedPorts = { "8080/tcp" = { }; };
  };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [
      project.packages.moog-antithesis-proxy
      pkgs.cacert
      pkgs.curl
      pkgs.coreutils
      pkgs.bash
    ];
  };
}
