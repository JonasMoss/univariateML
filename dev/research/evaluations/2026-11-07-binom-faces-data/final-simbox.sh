#!/bin/bash
set -uo pipefail
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 XDG_RUNTIME_DIR=$PWD/build/tmp
sha256sum dev/research/evaluations/2026-11-07-binom-faces-data/source-manifest.json
just cpp-test; a=$?
UMVL_TEST_TIER=full just cpp-test; b=$?
just cpp-test-no-long-double; c=$?
bash dev/research/evaluations/2026-11-07-binom-faces-data/run-check.sh; d=$?
python3 dev/research/evaluations/2026-11-07-binom-faces-data/check.py; e=$?
python3 dev/research/evaluations/2026-11-07-binom-faces-data/second-order-face.py; f=$?
python3 dev/research/evaluations/2026-11-07-binom-faces-data/higher-order.py; g=$?
bash dev/research/evaluations/2026-11-07-binom-faces-data/run-r-families.sh; h=$?
just test; i=$?
just test-full; j=$?
printf 'final_exit cpp_dev=%s cpp_full=%s noLD=%s probe=%s independent=%s second=%s higher=%s focusedR=%s Rdev=%s Rfull=%s\n' "$a" "$b" "$c" "$d" "$e" "$f" "$g" "$h" "$i" "$j"
exit $((a|b|c|d|e|f|g|h|i|j))
