{ mkDerivation, base, bytestring, Only, optparse-applicative
, semigroups, stdenv, system-filepath, text, time, transformers
, void
}:
mkDerivation {
  pname = "optparse-generic";
  version = "1.2.3";
  sha256 = "1wxzpj4xj3bafg3piarwsr69xxzp75fdglx9c3spbahl1aq9wzgk";
  libraryHaskellDepends = [
    base bytestring Only optparse-applicative semigroups
    system-filepath text time transformers void
  ];
  description = "Auto-generate a command-line parser for your datatype";
  license = stdenv.lib.licenses.bsd3;
}
