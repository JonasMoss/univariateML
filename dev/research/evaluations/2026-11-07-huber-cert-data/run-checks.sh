#!/bin/sh
# Run from the worktree root via /tmp/umvl-cpu/simbox-run sh <this-file>.
# The launcher supplies nice, CPU limits, and writable temporary directories.
set -eu
mkdir -p build/scratch
huber_finish() {
  huber_rc=$?
  trap - EXIT
  set +e
  tail -40 build/scratch/huber-cpp-full.log
  tail -40 build/scratch/huber-cpp-no-long-double.log
  tail -12 build/scratch/huber-r-dev.log
  tail -25 build/scratch/huber-r-full.log
  cat build/scratch/huber-check-final.log build/scratch/huber-budget-final.log build/scratch/huber-scale-final.log
  sha256sum univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp
  if [ -f univariateML-r/inst/include/cppunivariate/distributions/huber/mle.hpp ]; then
    sha256sum univariateML-r/inst/include/cppunivariate/distributions/huber/mle.hpp
  fi
  exit "$huber_rc"
}
for huber_log in huber-cpp-full huber-cpp-no-long-double huber-r-dev huber-r-full huber-check-final huber-budget-final huber-scale-final; do
  : > "build/scratch/$huber_log.log"
done
trap huber_finish EXIT
sha256sum univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp bindings/common/distributions/huber/fit.hpp bindings/r/distributions/huber/test.R univariateML-cpp/include/cppunivariate/distributions/huber/test.cpp
UMVL_TEST_TIER=full just cpp-test > build/scratch/huber-cpp-full.log 2>&1
just cpp-test-no-long-double > build/scratch/huber-cpp-no-long-double.log 2>&1
just test > build/scratch/huber-r-dev.log 2>&1
just test-full > build/scratch/huber-r-full.log 2>&1
huber_boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$huber_boost" dev/research/evaluations/2026-11-07-huber-cert-data/check.cpp -o build/scratch/huber-check
build/scratch/huber-check > build/scratch/huber-check-final.log
c++ -O2 -std=c++17 -IunivariateML-cpp/include -Ibindings/common dev/research/evaluations/2026-11-07-huber-cert-data/budget-search.cpp -o build/scratch/huber-budget
build/scratch/huber-budget > build/scratch/huber-budget-final.log
c++ -O2 -std=c++17 -DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS -IunivariateML-cpp/include -Ibindings/common dev/research/evaluations/2026-11-07-huber-cert-data/scale-probe.cpp -o build/scratch/huber-scale-probe
build/scratch/huber-scale-probe > build/scratch/huber-scale-final.log
