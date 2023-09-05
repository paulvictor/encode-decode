{
  description = "Flake system for encode-decode";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc962;
        profiledHaskellPackages = haskellPackages.extend(hf: hprev: {
          mkDerivation = args: hprev.mkDerivation {
            enableLibraryProfiling = true;
          };
        });
        withThisPackage = hfinal: hprev: {
          streamly-core = lib.doJailbreak hprev.streamly-core;
          encodeDecodeTest = hfinal.callCabal2nix "encode-decode" ./. {};
        };
        inherit (pkgs.haskell) lib;
      in {
        packages = rec {
          encodeDecodeTest = (haskellPackages.extend withThisPackage).encodeDecodeTest;
          encodeDecodeTestProfiled =
            lib.enableExecutableProfiling
              ((profiledHaskellPackages.extend withThisPackage).encodeDecodeTest);
          default = encodeDecodeTest;
        };
        devShells = rec {
          withoutProfiling = (haskellPackages.extend withThisPackage).shellFor {
            packages = p: [p.encodeDecodeTest];
          };
          withProfiling = (profiledHaskellPackages.extend withThisPackage).shellFor {
            packages = p: [p.encodeDecodeTest];
          };
          default = withoutProfiling;
        };

      }
    );
}
