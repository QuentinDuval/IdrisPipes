# This is used in the Travis build to install the Idris compiler.
let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in {
  IdrisReducers = stdenv.mkDerivation {
    name = "IdrisPipes";
    src = ./.;
    buildInputs = with pkgs; [ haskellPackages.idris gmp ];
  };
}
