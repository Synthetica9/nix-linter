{ pkgs ? import <nixpkgs> {} }:
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
