{ nixpkgsCommit ? "72b9660dc18ba347f7cd41a9504fc181a6d87dc3", nixpkgsURL ?
  "https://github.com/NixOS/nixpkgs/archive/${nixpkgsCommit}.tar.gz"
, pkgsPath ? builtins.fetchTarball nixpkgsURL, pkgs ? import pkgsPath { } }:
with pkgs;

(haskellPackages.override ({ overrides = self: super: { }; })).extend
(haskell.lib.packageSourceOverrides { nix-linter = ./.; }) // {
  inherit pkgs;
}
