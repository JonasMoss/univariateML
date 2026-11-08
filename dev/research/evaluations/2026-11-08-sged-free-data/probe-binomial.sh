#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
boost_include=$(Rscript -e 'cat(system.file("include",package="BH"))')
source_file=dev/research/evaluations/2026-11-08-sged-free-data/probe-binomial.cpp
g++ -std=c++17 -O2 -Ibindings -IunivariateML-cpp/include -I"$boost_include" "$source_file" -o build/scratch/binomial-baseline
build/scratch/binomial-baseline > dev/research/evaluations/2026-11-08-sged-free-data/binomial-baseline-native.tsv
g++ -std=c++17 -MM -Ibindings -IunivariateML-cpp/include -I"$boost_include" "$source_file" > build/scratch/binomial-baseline-deps.txt
python3 - <<'PY'
from pathlib import Path
assert '/sged/' not in Path('build/scratch/binomial-baseline-deps.txt').read_text()
PY
source "${EMSDK:-$HOME/emsdk}/emsdk_env.sh" >/dev/null
export EM_CACHE=$PWD/build/em-cache EMCC_CORES=1
em++ -std=c++17 -O2 -Ibindings -IunivariateML-cpp/include -I"$boost_include" "$source_file" -sENVIRONMENT=node -sSINGLE_FILE=1 -o build/scratch/binomial-baseline.mjs
node --input-type=module -e 'const {default:create}=await import("./build/scratch/binomial-baseline.mjs");await create();' > dev/research/evaluations/2026-11-08-sged-free-data/binomial-baseline-wasm.tsv
