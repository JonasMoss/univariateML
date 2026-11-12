#!/usr/bin/env bash
set -euo pipefail
here=dev/research/evaluations/2026-11-12-dpq-counts-data
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
if [[ ${1:-} != locked ]]; then exec /tmp/umvl-cpu/run4 bash "$here/late_checks.sh" locked; fi
run_check() {
 local label=$1; shift
 set +e
 "$@" > "$here/$label.log" 2>&1
 local rc=$?
 set -e
 printf '%s\tworkstation\t%s\n' "$label" "$rc" >> "$here/suites.tsv"
 tail -n 8 "$here/$label.log"
 return "$rc"
}
run_check cpp-bnbinom-final bash -c 'cmake --build univariateML-cpp/build -j4 && ctest --test-dir univariateML-cpp/build --output-on-failure -R "^distribution_bnbinom(_slow)?$"'
run_check cpp-no-long-double-bnbinom-final bash -c 'cmake --build build/cpp-no-long-double -j4 && ctest --test-dir build/cpp-no-long-double --output-on-failure -R "^distribution_bnbinom(_slow)?$"'
run_check r-bnbinom-cmp-final env UMVL_TEST_FAMILIES=bnbinom,cmp UMVL_TEST_TIER=full just test
run_check js-build-final just js-build
run_check js-bnbinom-cmp-final env UMVL_TEST_FAMILIES=bnbinom,cmp just js-test
cp build/scratch/truth-results.json "$here/js-targeted-truth-results.json"
run_check site-check-last just site-check
