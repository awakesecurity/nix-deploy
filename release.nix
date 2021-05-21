let
  pinnedNixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/673aea9f84c955c94b105797fdc56007017af4db.tar.gz";
    sha256 = "13zzzsjky30hyj2mm3m8pdna4qyajpa8c40aagx4w9rz1x5h4m6y";
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
