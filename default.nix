let
  pkgs =
    import
      (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/7c06b2145ddc21a20c7f178c3391bdaf8497fae2.tar.gz";
        sha256 = "1a0aaybapbcv39dvji0l138lvwimyr9skx5mz88y65ysf7zvlpwi";
      })
      { config = { }; };

in
pkgs.haskellPackages.callCabal2nix "nix-deploy" ./. { }
