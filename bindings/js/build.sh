#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/js
source "${EMSDK:-$HOME/emsdk}/emsdk_env.sh" >/dev/null
export EM_CACHE="$PWD/build/em-cache" EMCC_CORES="${EMCC_CORES:-4}"
python3 bindings/js/generate.py ${JS_FAMILIES:+--families "$JS_FAMILIES"}
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
em++ bindings/js/bindings.cpp -std=c++17 "-${1:-O2}" -Ibindings -Ibuild/js -IunivariateML-cpp/include -I"$boost" --bind -sMODULARIZE -sEXPORT_ES6 -sALLOW_MEMORY_GROWTH -sENVIRONMENT=web,node --emit-tsd "${2:-univariateML}.d.ts" "${@:3}" -o "build/js/${2:-univariateML}.mjs"
