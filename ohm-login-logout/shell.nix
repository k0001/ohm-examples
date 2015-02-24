with import <nixpkgs> {};
let haskellPackages = pkgs.haskellPackages_ghcjs.override {
      extension = self: super: {
        lei = self.callPackage ./deps/lei {};
        oHm = self.callPackage ./deps/ohm {};
      };
    };

in pkgs.callPackage ./. {
     cabal = haskellPackages.cabal.override {
       extension = self: super: {
         buildTools = super.buildTools ++ [ haskellPackages.ghc.ghc.parent.cabalInstall ];
       };
     };
     inherit (haskellPackages) ghcjsBase lei oHm lens basePrelude text pipes transformers;
   }
