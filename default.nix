{ mkDerivation, base, neat-interpolation, optparse-applicative
, optparse-generic, stdenv, text, turtle
}:
mkDerivation {
  pname = "nix-deploy";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base neat-interpolation optparse-applicative optparse-generic text
    turtle
  ];
  homepage = "https://github.com/awakesecurity/nix-deploy#readme";
  description = "Deploy Nix-built software to a NixOS machine";
  license = stdenv.lib.licenses.asl20;
}
