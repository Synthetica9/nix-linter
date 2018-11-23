{ pkgs ? import <nixpkgs> {} }:
with pkgs;

(haskellPackages.override ({
    overrides = self: super: {
      streamly = super.streamly_0_5_2;
      path-io = super.path-io_1_4_0;
    };
})).extend (haskell.lib.packageSourceOverrides {
  nix-linter = ./.;
})
// {
  inherit pkgs;
}
