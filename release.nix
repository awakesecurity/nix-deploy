let
  pinnedNixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/7c06b2145ddc21a20c7f178c3391bdaf8497fae2.tar.gz";
    sha256 = "1a0aaybapbcv39dvji0l138lvwimyr9skx5mz88y65ysf7zvlpwi";
  };

in

{ nixpkgs ? pinnedNixpkgs }:

let
  config = { };

  overlays = [
    (pkgsNew: pkgsOld: {
      haskellPackages = pkgsOld.haskellPackages.override (old: {
        overrides =
          let
            directoryOverride =
              pkgsNew.haskell.lib.packagesFromDirectory { directory = ./nix; };

          in
            pkgsNew.lib.composeExtensions
              (old.overrides or (_: _: { }))
              directoryOverride;
      });
    })
  ];

  pkgs = import nixpkgs { inherit config overlays; };

in
  { inherit (pkgs.haskellPackages) nix-deploy; }
