#!/usr/bin/env bash
# Reconstruct baseline headers in ignored scratch. Do not use an outer slot lock.
set -euo pipefail
if [[ ${1:-} == --worker ]]; then
  root=$2
  base=$3
  source "${EMSDK:-$HOME/emsdk}/emsdk_env.sh" >/dev/null
  export EM_CACHE="$root/build/em-cache" EMCC_CORES=4 BINARYEN_CORES=4
  boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
  affinity=()
  if command -v taskset >/dev/null; then
    cpus=$(python3 -c 'import os; print(",".join(map(str,sorted(os.sched_getaffinity(0))[:4])))')
    affinity=(taskset -c "$cpus")
  fi
  em++ --version
  "${affinity[@]}" em++ "$root/bindings/js/bindings.cpp" -std=c++17 -O2 -I"$root/bindings" -I"$root/build/js" -I"$base/univariateML-cpp/include" -I"$boost" --bind -sMODULARIZE -sEXPORT_ES6 -sALLOW_MEMORY_GROWTH -sENVIRONMENT=web,node -o "$root/build/js/baseline/univariateML.mjs"
  exit
fi
root=$(cd "$(dirname "$0")/../../../.." && pwd)
cd "$root"
base="$root/build/scratch/qtie-2-js-baseline"
mkdir -p "$base" "$root/build/tmp" "$root/build/js/baseline"
git archive c167a97580db656bba2e8153d970cd4de69c8f64 univariateML-cpp/include | tar -x -C "$base"
export TMPDIR="$root/build/tmp" JUST_TEMPDIR="$root/build/tmp" XDG_RUNTIME_DIR="$root/build/tmp"
exec /tmp/umvl-cpu/run4 bash "$0" --worker "$root" "$base"
