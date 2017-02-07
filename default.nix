{ mkDerivation, base, criu-rpc-types, lens-family, network, process
, proto-lens, stdenv, text, unix
}:
mkDerivation {
  pname = "criu-rpc";
  version = "0.0.2";
  src = ./.;
  libraryHaskellDepends = [
    base criu-rpc-types lens-family network process proto-lens text
    unix
  ];
  description = "CRIU RPC client";
  license = stdenv.lib.licenses.mit;
}
