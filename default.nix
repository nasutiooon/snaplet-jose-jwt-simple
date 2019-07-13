{ mkDerivation, aeson, base, bytestring, configurator, cryptonite
, hpack, jose-jwt, mtl, snap, stdenv, time
}:
mkDerivation {
  pname = "snaplet-jose-jwt-simple";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring configurator cryptonite jose-jwt mtl snap
    time
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/nasutiooon/snaplet-jose-jwt-simple#readme";
  license = stdenv.lib.licenses.bsd3;
}
