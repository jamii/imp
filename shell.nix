{ }:

let

  pkgs = import (builtins.fetchTarball {
    name = "nixos-21.11";
    url = "https://github.com/NixOS/nixpkgs/archive/21.11.tar.gz";
    sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
  }) {};

  zig = pkgs.stdenv.mkDerivation {
      name = "zig";
      src = fetchTarball (
          if (pkgs.system == "x86_64-linux") then {
              url = "https://ziglang.org/download/0.9.0/zig-linux-x86_64-0.9.0.tar.xz";
              sha256 = "1vagp72wxn6i9qscji6k3a1shy76jg4d6crmx9ijpch9kyn71c96";
          } else if (pkgs.system == "aarch64-linux") then {
              url = "https://ziglang.org/download/0.9.0/zig-linux-aarch64-0.9.0.tar.xz";
              sha256 = "00m6nxp64nf6pwq407by52l8i0f2m4mw6hj17jbjdjd267b6sgri";
          } else 
          throw ("Unknown system " ++ pkgs.system)
      );
      dontConfigure = true;
      dontBuild = true;
      installPhase = ''
          mkdir -p $out
          mv ./* $out/
          mkdir -p $out/bin
          mv $out/zig $out/bin
      '';
  };

in

pkgs.mkShell rec {
  buildInputs = [
    zig
    
    # gui stuff
    pkgs.pkg-config
    pkgs.libGL.all
    pkgs.xorg.libX11.all
    pkgs.xorg.libXrandr.all
    pkgs.xorg.libXcursor
    pkgs.xorg.libXinerama
    pkgs.xorg.xinput
    pkgs.xlibs.xorgproto
    pkgs.xlibs.libXi.all
    pkgs.xlibs.libXext.all
    pkgs.glew.all
  ];
}