with import ./. {};

shellFor {
  packages = p: with p; [ nix-linter ];
  withHoogle = true;
  nativeBuildInputs = [ cabal-install ];
}
