#!/usr/bin/env bash
set -euo pipefail
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
evidence=dev/research/evaluations/2026-11-08-st-cert-data
c++ -std=c++17 -O1 -IunivariateML-cpp/include -I"$boost" "$evidence/probe.cpp" -o build/scratch/st-probe
build/scratch/st-probe > "$evidence/probe.txt"
python3 "$evidence/check.py"
c++ -std=c++17 -O0 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" -DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\" univariateML-cpp/tests/distribution_tests_main.cpp univariateML-cpp/include/cppunivariate/distributions/st/test.cpp -o build/scratch/st-tests
build/scratch/st-tests '--test-case=st:*'
