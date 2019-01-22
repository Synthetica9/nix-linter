{ pkgs ? import (builtins.fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/e6d726e5aa527e6f41ec06adeb7f0272294373c8.tar.gz;
    sha256 = "0s09nfcl245pyyyqi9q8m1lfaiqms5sm81lcpmvvsh5988iylcfx";
  }) { }
}:
with pkgs;

(haskellPackages.override ({
    overrides = self: super: {
      streamly = super.streamly_0_5_2 or super.streamly_0_5_0 or super.streamly;
      path-io = super.path-io_1_4_1 or super.path-io;
    };
})).extend (haskell.lib.packageSourceOverrides {
  nix-linter = ./.;
})
// {
  inherit pkgs;
}
