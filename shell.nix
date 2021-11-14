{ }:

let

  pkgs = import (builtins.fetchTarball {
    name = "nixos-20.09";
    url = "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz";
    sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
  }) {};

  zig = pkgs.stdenv.mkDerivation {
        name = "zig";
        src = fetchTarball (if (pkgs.system == "x86_64-linux") then {
            url = "https://ziglang.org/builds/zig-linux-x86_64-0.9.0-dev.1625+d3a099c14.tar.xz";
            sha256 = "1azv16jkn7mh1jm9m6k7d5nkjjs26dhy26azg9xcb4hg5qrznnc0";
        } else throw ("Unknown system " ++ pkgs.system));
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

pkgs.mkShell rec {
  buildInputs = [
    zig
  ];
}
