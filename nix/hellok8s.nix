{ lib, mkDerivation, fetchFromGitHub, aeson, base, bytestring, hostname, hpack, http-media
, http-types, lucid, monad-logger, mtl, servant, servant-docs
, servant-lucid, servant-server, stdenv, text, unordered-containers
, wai, wai-extra, warp
}:
let
  gitignoreSrc = fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    # put the latest commit sha of gitignore Nix library here:
    rev = "f9e996052b5af4032fe6150bba4a6fe4f7b9d698";
    # use what nix suggests in the mismatch message here:
    sha256 = "sha256:0jrh5ghisaqdd0vldbywags20m2cxpkbbk5jjjmwaw0gr8nhsafv";
  };
  inherit (import gitignoreSrc {}) gitignoreSource;
in
mkDerivation {
  pname = "hellok8s";
  version = "0.1.0.0";
  src = gitignoreSource ../hellok8s;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring hostname http-media http-types lucid monad-logger mtl
    servant servant-docs servant-lucid servant-server text
    unordered-containers wai wai-extra warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base hostname http-media http-types lucid monad-logger mtl
    servant servant-docs servant-lucid servant-server text
    unordered-containers wai wai-extra warp
  ];
  prePatch = "hpack";
  doHaddock = false;
  enableSharedExecutables = false;
  enableLibraryProfiling = false;
  homepage = "https://github.com/denibertovic/hellok8s#readme";
  license = lib.licenses.gpl3;
}
