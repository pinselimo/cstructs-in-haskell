{
  description = "C-Struct Types for Haskell";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages = hsPkgs : with hsPkgs; [
        # Package
        base template-haskell
        # Testing
        doctest Glob HUnit QuickCheck test-framework
        test-framework-hunit test-framework-quickcheck2
      ];
      fDrv = { mkDerivation, base, doctest, Glob, HUnit, lib, QuickCheck
          , template-haskell, test-framework, test-framework-hunit
          , test-framework-quickcheck2
          }:
        mkDerivation {
          pname = "C-structs";
          version = "0.2.0.3";
          src = ./.;
          libraryHaskellDepends = [ base template-haskell ];
          testHaskellDepends = [
            base doctest Glob HUnit QuickCheck template-haskell test-framework
            test-framework-hunit test-framework-quickcheck2
          ];
          homepage = "https://github.com/pinselimo/cstructs-in-haskell#readme";
          description = "C-Structs implementation for Haskell";
          license = lib.licenses.mit;
        };
    in {
      packages.default  = pkgs.haskellPackages.callPackage fDrv {};
      devShells.default = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          (ghc.withPackages haskellPackages)
          cabal-install
        ];
        buildInputs = [ ];
      };
    });
}
