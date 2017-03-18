{ mkDerivation, base, random, reactive-banana, stdenv }:
mkDerivation {
  pname = "eve-eb-compare";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base random reactive-banana ];
  license = stdenv.lib.licenses.bsd3;
}
