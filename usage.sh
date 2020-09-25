set -euxo pipefail

$(nix-build -A nix-linter)/bin/nix-linter --help
