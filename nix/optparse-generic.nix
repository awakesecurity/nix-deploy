{ mkDerivation, base, bytestring, Only, optparse-applicative
, semigroups, stdenv, system-filepath, text, time, transformers
, void
}:
mkDerivation {
  pname = "optparse-generic";
  version = "1.2.2";
  sha256 = "110jil2n945x30d8wgdrgs7di310z9hdnzhw5c1zq2jfh3b54ygz";
  libraryHaskellDepends = [
    base bytestring Only optparse-applicative semigroups
    system-filepath text time transformers void
  ];
  description = "Auto-generate a command-line parser for your datatype";
  license = stdenv.lib.licenses.bsd3;
}
