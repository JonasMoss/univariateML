#!/usr/bin/env bash
# Run through /tmp/umvl-cpu/run from the repository root (one thread).
set -uo pipefail
evidence=dev/research/evaluations/2026-11-13-render-data
failed=0
Rscript -e 'pkgload::load_all("univariateML-r",quiet=TRUE,recompile=FALSE); testthat::test_file("bindings/r/tests/testthat/test-outcomes.R",stop_on_failure=TRUE)' > "$evidence/r-outcomes-final.log" 2>&1
code=$?
printf 'R-outcomes\t%s\tworkstation\n' "$code" > "$evidence/targeted-checks.tsv"
if [[ "$code" != 0 ]]; then failed=1; fi
node --test --test-name-pattern='uncertified limits describe the likelihood margin' bindings/js/stop-test.mjs > "$evidence/js-margin-final.log" 2>&1
code=$?
printf 'JS-margin\t%s\tworkstation\n' "$code" >> "$evidence/targeted-checks.tsv"
if [[ "$code" != 0 ]]; then failed=1; fi
exit "$failed"
