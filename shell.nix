let pkgs = (import <nixpkgs> {});
    haskellPackages = pkgs.recurseIntoAttrs (pkgs.haskellPackages.override {
        extension = self : super :
        let callPackage = self.callPackage;
        in {
            thisPackage = haskellPackages.callPackage (import ./default.nix) {};
        };
    });
    hsEnv = pkgs.haskellPackages.ghcWithPackages (hsPkgs : ([
        hsPkgs.hlint
        hsPkgs.pointfree
        hsPkgs.hdevtools
        hsPkgs.hasktags
    ]));
in pkgs.lib.overrideDerivation haskellPackages.thisPackage (old: {
    buildInputs = old.buildInputs ++ [
        haskellPackages.cabalInstall
        pkgs.lftp
        pkgs.openssl
        pkgs.imagemagick
        hsEnv
    ];
    extraCmds = ''
        $(grep export ${hsEnv.outPath}/bin/ghc)
    '';
    })
