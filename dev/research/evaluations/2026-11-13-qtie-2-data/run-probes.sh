#!/usr/bin/env bash
# Rebuild the measured starting commit or current tree; no source archive is kept.
set -euo pipefail
root=$(cd "$(dirname "$0")/../../../.." && pwd)
cd "$root"
evidence="$root/dev/research/evaluations/2026-11-13-qtie-2-data"
phase=${1:?usage: run-probes.sh before|after}
case "$phase" in before|after) ;; *) exit 2 ;; esac
scratch="$root/build/scratch/qtie-2-replay-$phase"
mkdir -p "$scratch" "$root/build/tmp"
export TMPDIR="$root/build/tmp" JUST_TEMPDIR="$root/build/tmp" XDG_RUNTIME_DIR="$root/build/tmp"
export QTIE2_EVIDENCE_DIR="$evidence"
source_root="$root"
if [[ $phase == before ]]; then
  source_root="$scratch/source"
  mkdir -p "$source_root"
  git archive c167a97580db656bba2e8153d970cd4de69c8f64 | tar -x -C "$source_root"
fi
boost=$(/tmp/umvl-cpu/run Rscript -e 'cat(system.file("include",package="BH"))')
/tmp/umvl-cpu/run python3 "$evidence/generate.py"
/tmp/umvl-cpu/run bash -e -o pipefail -c '
  c++ -std=c++17 -O2 -I"$1/univariateML-cpp/include" -I"$2" "$3/probe.cpp" -o "$4/probe"
  "$4/probe" > "$3/cpp-$5.csv"
  c++ -std=c++17 -O2 -I"$1/univariateML-cpp/include" -I"$2" "$3/samplers.cpp" -o "$4/samplers"
  "$4/samplers" | gzip > "$3/samplers-$5.csv.gz"
' _ "$source_root" "$boost" "$evidence" "$scratch" "$phase"
/tmp/umvl-cpu/run4 bash -ec 'cd "$1"; MAKEFLAGS=-j4 just regen' _ "$source_root"
/tmp/umvl-cpu/run bash -ec 'cd "$1"; Rscript "$2/probe.R" "$2/r-$3.csv" "$2/r-samplers-$3.rds"' _ "$source_root" "$evidence" "$phase"
/tmp/umvl-cpu/run python3 "$evidence/summarize.py"
