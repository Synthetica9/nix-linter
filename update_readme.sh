#! /usr/bin/env nix-shell
#! nix-shell -i bash -p gpp bash

# See https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# Fixes troubles with Travis WRT insufficient disk space
unset TMPDIR

gpp -H -x README.md.gpp -o README.md
