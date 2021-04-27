{ }:

let

  nixpkgs = builtins.fetchTarball {
    name = "nixos-20.09";
    url = "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz";
    sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
  };

  hostPkgs = import nixpkgs {};

  zig = hostPkgs.stdenv.mkDerivation {
    name = "zig";
    src = fetchTarball {
        url = "https://ziglang.org/download/0.7.1/zig-linux-x86_64-0.7.1.tar.xz";
        sha256 = "1jpp46y9989kkzavh73yyd4ch50sccqgcn4xzcflm8g96l3azl40";
    };
    dontConfigure = true;
    dontBuild = true;
    installPhase = ''
      mkdir -p $out
      mv ./lib $out/
      mkdir -p $out/bin
      mv ./zig $out/bin
      mkdir -p $out/doc
      #mv ./langref.html $out/doc
    '';
  };

in

hostPkgs.mkShell rec {
  buildInputs = [
    zig
  ];
}
