{ mkDerivation, base, containers, hspec, hspec-discover, lib
, megaparsec, prettyprinter, prettyprinter-ansi-terminal
, prettyprinter-compat-ansi-wl-pprint, QuickCheck, scientific
, semigroups, text, transformers
}:
mkDerivation {
  pname = "language-thrift";
  version = "0.12.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base containers megaparsec prettyprinter-compat-ansi-wl-pprint
    scientific semigroups text transformers
  ];
  testHaskellDepends = [
    base containers hspec megaparsec prettyprinter
    prettyprinter-ansi-terminal prettyprinter-compat-ansi-wl-pprint
    QuickCheck scientific semigroups text transformers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/abhinav/language-thrift#readme";
  description = "Parser and pretty printer for the Thrift IDL format";
  license = lib.licenses.bsd3;
}
