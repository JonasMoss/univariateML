#!/usr/bin/env bash
# Called inside one CPU reservation. No nested slot locks; all builds block.
set -euo pipefail
validation_data=dev/research/evaluations/2026-11-07-tlambda-impl-data
mkdir -p build/scratch
cat > build/scratch/test-main.cpp <<'CPP'
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
CPP
sha256sum -c "$validation_data/source-manifest.sha256" > build/scratch/source-before-complete.log
dist_test_root="$PWD/univariateML-cpp/include/cppunivariate/distributions"
c++ -std=c++17 -O1 -IunivariateML-cpp/include -IunivariateML-cpp/tests "-DCPPUNIV_DISTRIBUTIONS_DIR=\"$dist_test_root\"" build/scratch/test-main.cpp univariateML-cpp/include/cppunivariate/distributions/tlambda/test.cpp -o build/scratch/validation-final
build/scratch/validation-final '--test-case-exclude=*every held golden subset*' > build/scratch/target-complete.log 2>&1
c++ -std=c++17 -O2 -I. -IunivariateML-cpp/include "$validation_data/checks.cpp" -o build/scratch/checks-verified
python3 "$validation_data/check.py" build/scratch/checks-verified > build/scratch/checks-complete.log 2>&1
c++ -std=c++17 -O2 -IunivariateML-cpp/include "$validation_data/golden-check.cpp" -o build/scratch/golden-check-verified
python3 "$validation_data/golden-check.py" build/scratch/golden-check-verified > build/scratch/golden-check-complete.log 2>&1
c++ -std=c++17 -O2 -IunivariateML-cpp/include "$validation_data/box-probe.cpp" -o build/scratch/box-probe-verified
build/scratch/box-probe-verified > "$validation_data/box-probe.json"
build/scratch/box-probe-verified negative > "$validation_data/negative-box-probe.json"
c++ -std=c++17 -O2 -IunivariateML-cpp/include "$validation_data/timing.cpp" -o build/scratch/timing-verified
sha256sum univariateML-cpp/include/cppunivariate/distributions/tlambda/{mle,certificate}.hpp > "$validation_data/timing-source.sha256"
build/scratch/timing-verified > "$validation_data/timing-after.csv"
sha256sum -c "$validation_data/source-manifest.sha256" > build/scratch/source-complete.log
printf 'native-validation,workstation,0\n' > build/scratch/native-complete.result
