#!/usr/bin/env bash
set -eu
cd "$(dirname "$0")/../../../.."
prefix="${1:?supply receipt prefix}"
shift
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch/oracle-consumers
/tmp/umvl-cpu/run bash dev/research/evaluations/2026-11-11-oracle-gaps-data/run_consumers.sh "$prefix" "$@"
