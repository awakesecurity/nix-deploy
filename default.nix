let
  rev = "9899f2b5602a6a8be6e79725d39856025861a5d0";

  sha256 = "0zb05xxhy1zynpf9014a3jrv6hibs0zz855vc8x4x5hcjpx34dlc";

  pkgs =
    import
      (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
        inherit sha256;
      })
      { config = { }; };

in
pkgs.haskellPackages.callCabal2nix "nix-deploy" ./. { }
