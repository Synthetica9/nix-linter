{ nixpkgsCommit ? "d16a5a5916852d54ba60ca1c408d52786f38aa67"
, nixpkgsURL ? "https://github.com/NixOS/nixpkgs/archive/${nixpkgsCommit}.tar.gz"
, pkgsPath ? builtins.fetchTarball nixpkgsURL
, pkgs ? import pkgsPath {}
}:
with pkgs;

(haskellPackages.override ({
    overrides = self: super: {
      streamly = super.streamly_0_5_2 or super.streamly_0_5_0 or super.streamly;
      path-io = super.path-io_1_4_0 or super.path-io;
    };
})).extend (haskell.lib.packageSourceOverrides {
  nix-linter = ./.;
})
// {
  inherit pkgs;
}
