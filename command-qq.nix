{ mkDerivation, base, doctest, hspec, process, stdenv
, template-haskell, text
}:
mkDerivation {
  pname = "command-qq";
  version = "0.3.0.0";
  src = ./.;
  buildDepends = [ base process template-haskell text ];
  testDepends = [ base doctest hspec template-haskell text ];
  homepage = "http://biegunka.github.io/command-qq/";
  description = "Quasiquoters for external commands";
  license = stdenv.lib.licenses.bsd3;
}
