let
  config = { allowUnfree = true;
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          optparse-applicative =
            pkgs.haskell.lib.dontCheck
              (haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { });

          optparse-generic =
            haskellPackagesNew.callPackage ./nix/optparse-generic.nix { };

          Only =
            haskellPackagesNew.callPackage ./nix/Only.nix { };

          turtle =
            haskellPackagesNew.callPackage ./nix/turtle.nix { };

          nix-deploy =
            haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
  { inherit (pkgs.haskellPackages) nix-deploy; }
