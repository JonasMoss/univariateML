#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp" PYTHONDONTWRITEBYTECODE=1
export CCACHE_DIR="${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}"
mkdir -p "$TMPDIR" build/scratch/fit-audit
/tmp/umvl-cpu/run bash -euo pipefail -c '
python3 dev/research/evaluations/2026-11-09-fit-audit-rerun-data/audit.py generate
boost=$(Rscript -e '\''cat(system.file("include",package="BH"))'\'')
ccache c++ -std=c++17 -O1 -IunivariateML-cpp/include -I"$boost" build/scratch/fit-audit/stress.cpp -o build/scratch/fit-audit/stress
'
