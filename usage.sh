set -euxo pipefail

$(nix-build --no-out-link)/bin/nix-linter --help
