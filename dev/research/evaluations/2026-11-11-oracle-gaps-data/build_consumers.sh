#!/usr/bin/env bash
set -u
cd "$(dirname "$0")/../../../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch/oracle-consumers
failed=0
tag="${1:-}"
args=()
if [ -n "$tag" ]; then args=(native "$tag"); fi
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-11-oracle-gaps-data/audit.py build after "${args[@]}" >"build/scratch/oracle-consumers/build-$tag-native.log" 2>&1
rc=$?
echo "native build $rc"
if [ "$rc" != 0 ]; then failed=1; fi
args=(no-long-double)
if [ -n "$tag" ]; then args+=("$tag"); fi
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-11-oracle-gaps-data/audit.py build after "${args[@]}" >"build/scratch/oracle-consumers/build-$tag-double.log" 2>&1
rc=$?
echo "double build $rc"
if [ "$rc" != 0 ]; then failed=1; fi
exit "$failed"
