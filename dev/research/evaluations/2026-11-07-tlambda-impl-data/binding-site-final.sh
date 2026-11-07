#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/scratch/tlambda-theory
rm -f build/scratch/js-build-complete.result build/scratch/js-test-complete.result build/scratch/site-complete.result
/tmp/umvl-cpu/run4 bash -c 'sha256sum univariateML-cpp/include/cppunivariate/distributions/tlambda/{mle,certificate}.hpp bindings/common/distributions/tlambda/fit.hpp bindings/js/stop-test.mjs > dev/research/evaluations/2026-11-07-tlambda-impl-data/js-source.sha256; just js-build' > build/scratch/js-build-complete.log 2>&1
printf 'js-build,workstation,0\n' > build/scratch/js-build-complete.result
/tmp/umvl-cpu/run just js-test > build/scratch/js-test-complete.log 2>&1
printf 'js-test,workstation,0\n' > build/scratch/js-test-complete.result
validation_cpus=$(python3 -c 'import os; print(",".join(map(str,sorted(os.sched_getaffinity(0))[:4])))')
/tmp/umvl-cpu/run4 taskset -c "$validation_cpus" env THEORY_CAPTURE=../../../../build/scratch/tlambda-theory/after GOMAXPROCS=1 just site-check > build/scratch/site-complete.log 2>&1
printf 'site-check,workstation,0\n' > build/scratch/site-complete.result
