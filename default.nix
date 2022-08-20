{ mkDerivation, aeson, base, base64-bytestring, bytestring
, containers, cryptonite, higher-leveldb, hspec, http-client
, http-types, lib, memory, mtl, network-simple, process, rio
, servant-server, stm, transformers, transformers-base, unix
, utf8-string, wai, warp, yaml
}:
mkDerivation {
  pname = "haskell-chain";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring containers cryptonite
    higher-leveldb memory mtl network-simple rio servant-server stm
    transformers transformers-base unix utf8-string wai warp
  ];
  executableHaskellDepends = [
    aeson base base64-bytestring bytestring containers cryptonite
    higher-leveldb http-client http-types memory mtl network-simple
    process rio servant-server stm transformers transformers-base unix
    utf8-string wai warp yaml
  ];
  testHaskellDepends = [
    aeson base base64-bytestring bytestring containers cryptonite
    higher-leveldb hspec memory mtl network-simple rio servant-server
    stm transformers transformers-base unix utf8-string wai warp
  ];
  homepage = "https://github.com/githubuser/haskell-chain#readme";
  license = lib.licenses.bsd3;
}
