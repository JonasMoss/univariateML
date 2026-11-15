#!/usr/bin/env bash
# Run through run4. Targeted late checks, then the not-yet-run required suites.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
D=dev/research/evaluations/2026-11-14-core5b-b-data
check() {
  local name="$1"; shift
  date -Is > "$D/$name.log"
  if "$@" >> "$D/$name.log" 2>&1; then
    printf '%s\tPASS\tworkstation\t%s\n' "$name" "$(date -Is)" >> "$D/suite-results.tsv"
    echo "$name PASS"
  else
    printf '%s\tFAIL\tworkstation\t%s\n' "$name" "$(date -Is)" >> "$D/suite-results.tsv"
    tail -60 "$D/$name.log"
    exit 1
  fi
  date -Is >> "$D/$name.log"
}
check cpp-invparalogis-targeted bash -c 'cmake --build univariateML-cpp/build -j4 --target distribution_tests && ctest --test-dir univariateML-cpp/build --output-on-failure -R "^distribution_invparalogis(_slow)?$"'
check cpp-double-invparalogis-targeted bash -c 'cmake --build build/cpp-no-long-double -j4 --target distribution_tests && ctest --test-dir build/cpp-no-long-double --output-on-failure -R "^distribution_invparalogis(_slow)?$"'
check regen-late just regen
check r-dev-targeted bash -c 'cd univariateML-r && UMVL_DISTRIBUTIONS_DIR="$OLDPWD/univariateML-cpp/include/cppunivariate/distributions" UMVL_TEST_TIER=dev PKG_BUILD_EXTRA_FLAGS=false TESTTHAT_CPUS=4 Rscript -e '\''options(pkg.build_extra_flags=FALSE);testthat::test_local(".",filter="^dist-(binom|invparalogis)$",stop_on_failure=TRUE)'\'''
bash "$D/run-suites.sh" test-full js-build js-test site-check
