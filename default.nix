{ nixpkgsSrc ? builtins.fetchTarball {
  url =
    "https://github.com/NixOS/nixpkgs/archive/b1f87ca164a9684404c8829b851c3586c4d9f089.tar.gz"; # haskell-updates
  sha256 = "0xzj6skg7vlip9h2bhs88kv15qjjqlhning26gs6c9628yk5zzcj";
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
