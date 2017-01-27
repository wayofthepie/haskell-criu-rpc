let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      thispackage = self.callPackage ./. {};
    };
  };
in pkgs.myEnvFun {
  name = haskellPackages.thispackage.name;
  buildInputs = [
     pkgs.pkgconfig
     pkgs.protobuf
     (haskellPackages.ghcWithPackages (hs: ([
     hs.cabal-install
      hs.hscolour
      hs.ghc-mod
    ] ++ hs.thispackage.propagatedNativeBuildInputs)))
  ];
}
