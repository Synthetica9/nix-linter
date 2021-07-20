{ nixpkgsSrc ? builtins.fetchTarball {
  url =
    "https://github.com/NixOS/nixpkgs/archive/38296d89d41c26a127b69c52421fbee95ceb0d22.tar.gz"; # haskell-updates
  sha256 = "108c4wm4vfqkgd6awpaskakq26f8ajx729s4bxqvvvfflrzwrlrv";
}, pkgs ? import nixpkgsSrc { }, compiler ? null }:

let
  haskellPackages = if compiler == null then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage {
  name = "";
  overrides = self: super: {
    streamly = self.streamly_0_8_0;
  };
  root = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
}
