{ nixpkgsSrc ? builtins.fetchTarball {
  url =
    "https://github.com/NixOS/nixpkgs/archive/51bb9f3e9ab6161a3bf7746e20b955712cef618b.tar.gz"; # nixpkgs-unstable
  sha256 = "1bqla14c80ani27c7901rnl37kiiqrvyixs6ifvm48p5y6xbv1p7";
}, pkgs ? import nixpkgsSrc { }, compiler ? null }:

let
  haskellPackages = if compiler == null then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage {
  name = "";
  root = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
}
