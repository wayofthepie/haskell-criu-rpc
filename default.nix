{ mkDerivation, base, criu-rpc-types, lens-family, network, process
, proto-lens, stdenv, text, unix
}:
mkDerivation {
  pname = "criu-rpc";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base criu-rpc-types lens-family network process proto-lens text
    unix
  ];
  license = stdenv.lib.licenses.mit;
}
