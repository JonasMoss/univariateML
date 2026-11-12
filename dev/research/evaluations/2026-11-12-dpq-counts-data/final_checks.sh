#!/usr/bin/env bash
# Try long suites on simbox first; reserve one local group for all fallbacks
# and workstation-only checks. No waits or nested slot reservations inside it.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
here=dev/research/evaluations/2026-11-12-dpq-counts-data
if [[ ${1:-} == local ]]; then
  run_local() {
    local label=$1; shift
    set +e
    "$@" > "$here/$label.log" 2>&1
    local rc=$?
    set -e
    printf '%s\tworkstation\t%s\n' "$label" "$rc" >> "$here/suites.tsv"
    tail -n 8 "$here/$label.log"
    return "$rc"
  }
  run_local cpp-pig-final bash -c 'cmake --build univariateML-cpp/build -j4 && univariateML-cpp/build/tests/distribution_tests "--source-file=*distributions/pig/test.cpp" "--test-case=*extreme count-tail regression*,*representable large integration coordinate*"'
  run_local cpp-no-long-double-gcc15 just cpp-test-no-long-double
  [[ $2 != 1 ]] || run_local r-dev just test
  [[ $3 != 1 ]] || run_local r-full just test-full
  run_local js-build just js-build
  run_local js-test just js-test
  run_local site-check-final just site-check
  exit
fi
r_dev_local=0
r_full_local=0
for recipe in cpp-test-no-long-double test test-full; do
  case "$recipe" in
    cpp-test-no-long-double) label=cpp-no-long-double-simbox;;
    test) label=r-dev-simbox;;
    test-full) label=r-full-simbox;;
  esac
  if [[ -f "$here/suites.tsv" ]] && awk -F '\t' -v label="$label" '$1==label && $2=="simbox" && $3==0 {found=1} END {exit !found}' "$here/suites.tsv"; then
    continue
  fi
  set +e
  /tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 just "$recipe" > "$here/$label.log" 2>&1
  rc=$?
  set -e
  if [[ $rc == 75 ]]; then
    printf '%s\tsimbox-unreachable\t75\n' "$label" >> "$here/attempts.tsv"
    [[ $recipe != test ]] || r_dev_local=1
    [[ $recipe != test-full ]] || r_full_local=1
  else
    printf '%s\tsimbox\t%s\n' "$label" "$rc" >> "$here/suites.tsv"
    tail -n 8 "$here/$label.log"
    [[ $rc == 0 ]] || exit "$rc"
  fi
done
[[ ${1:-} != remote-only ]] || exit 0
/tmp/umvl-cpu/run4 bash "$here/final_checks.sh" local "$r_dev_local" "$r_full_local"
