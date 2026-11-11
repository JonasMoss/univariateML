#!/usr/bin/env bash
# Usage: /tmp/umvl-cpu/run4 bash .../run-checks.sh <just target>
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
just "$@"
