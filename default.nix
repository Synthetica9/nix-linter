{ nixpkgsCommit ? "4c7a9a0a88ea6e8efe43661f215e0c5d32504f2e"
, nixpkgsURL ? "https://github.com/NixOS/nixpkgs/archive/${nixpkgsCommit}.tar.gz"
, pkgsPath ? builtins.fetchTarball nixpkgsURL
, pkgs ? import pkgsPath {}
}:
with pkgs;

(haskellPackages.override ({
    overrides = self: super: {
    };
})).extend (haskell.lib.packageSourceOverrides {
  nix-linter = ./.;
})
// {
  inherit pkgs;
}
