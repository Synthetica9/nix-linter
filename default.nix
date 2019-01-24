{ nixpkgsCommit ? "7f85bfd70d591ff7f369345ae602a92c1a633722"
, nixpkgsURL ? "https://github.com/NixOS/nixpkgs-channels/archive/${nixpkgsCommit}.tar.gz"
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
