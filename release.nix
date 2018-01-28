let
  config   = { allowUnfree = true; };
  overlays = [
    (newPkgs: oldPkgs: {
      haskellPackages = oldPkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          optparse-applicative =
            oldPkgs.haskell.lib.dontCheck
              (haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { });

          optparse-generic =
            haskellPackagesNew.callPackage ./nix/optparse-generic.nix { };

          nix-deploy =
            haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    })
  ];

  nixpkgs = import ./nix/17_09.nix;

  pkgs = import nixpkgs { inherit config overlays; };
in
  { inherit (pkgs.haskellPackages) nix-deploy; }
