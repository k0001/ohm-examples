{ basePrelude
, cabal
, ghcjsBase
, lei
, oHm
, lens
, pipes
, text
, transformers
}:

cabal.mkDerivation (self: {
  pname = "example-login-logout-web";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    basePrelude
    ghcjsBase
    lens
    lei
    oHm
    pipes
    text
    transformers
  ];
  doCheck = false;
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
