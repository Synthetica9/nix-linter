{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
  nix-linter = ./.;
})
// { inherit pkgs; }
