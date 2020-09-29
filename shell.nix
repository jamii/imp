{ }:

let

  nixpkgs = builtins.fetchTarball {
    name = "nixos-20.03";
    url = "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz";
    sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  };

  hostPkgs = import nixpkgs {};

  zig = hostPkgs.stdenv.mkDerivation {
    name = "zig";
    src = fetchTarball {
      url = "https://ziglang.org/builds/zig-linux-x86_64-0.6.0+e60939bfa.tar.xz";
      sha256 = "0rw8vmhrpniwwd6wvckc8vsbrasf37c7hq426z1pip5yk8b10byh";
    };
    dontConfigure = true;
    dontBuild = true;
    installPhase = ''
      mkdir -p $out
      mv ./lib $out/
      mkdir -p $out/bin
      mv ./zig $out/bin
      mkdir -p $out/doc
      mv ./langref.html $out/doc
    '';
  };

in

hostPkgs.mkShell rec {
  buildInputs = [
    zig
  ];
}
