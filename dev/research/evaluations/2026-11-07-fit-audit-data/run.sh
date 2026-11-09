#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch/fit-audit
export PYTHONDONTWRITEBYTECODE=1 CCACHE_DIR="${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}"
audit=dev/research/evaluations/2026-11-07-fit-audit-data/audit.py
/tmp/umvl-cpu/run python3 "$audit" generate
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-fit-audit-data/check_reference.py
boost=$(/tmp/umvl-cpu/run Rscript -e 'cat(system.file("include",package="BH"))')
/tmp/umvl-cpu/run ccache c++ -std=c++17 -O1 -IunivariateML-cpp/include -I"$boost" build/scratch/fit-audit/stress.cpp -o build/scratch/fit-audit/stress
/tmp/umvl-cpu/run python3 "$audit" fit
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-fit-audit-data/retry_fit.py
REFERENCE_SECONDS=20 /tmp/umvl-cpu/run python3 "$audit" reference
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-fit-audit-data/check_findings.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-fit-audit-data/focused_references.py
/tmp/umvl-cpu/run python3 "$audit" static
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-fit-audit-data/finalize.py
