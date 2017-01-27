{ pkgs ? import <nixpkgs> {} }:
let
   stdenv = pkgs.stdenv;
in stdenv.mkDerivation {
  name = "criu-rpc";
  src = "./";
  buildInputs = with pkgs; [
    cabal-install
    ghc
    stack
    haskellPackages.ghc-mod
    protobuf
  ];
  installPhase = ''
    mkdir -p $out
    cp -r * $out/
  '';
}
