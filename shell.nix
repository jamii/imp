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
      url = "https://ziglang.org/builds/zig-linux-x86_64-0.6.0+477798b37.tar.xz";
      sha256 = "07n54s0fyqvl9c808kb5dfk292rn5bw24j5qyzkzadwzw7imxrq5";
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
