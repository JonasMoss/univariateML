#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CCACHE_DIR=/home/jonas/.cache/ccache/univariateML CCACHE_BASEDIR=$PWD CCACHE_NOHASHDIR=1
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
out=dev/research/evaluations/2026-11-08-st-cert-data
python3 "$out/check.py" > "$out/check-workstation.txt"
ccache c++ -std=c++17 -O0 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" -DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\" univariateML-cpp/tests/distribution_tests_main.cpp univariateML-cpp/include/cppunivariate/distributions/st/test.cpp -o build/scratch/st-tests
build/scratch/st-tests '--test-case=st:*' > "$out/targeted-workstation.txt"
ccache c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" "$out/timing.cpp" -o build/scratch/st-after
build/scratch/st-after > "$out/timing-after.txt"
ccache c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" "$out/extra-timing.cpp" -o build/scratch/st-extra-after
build/scratch/st-extra-after > "$out/extra-timing-after.txt"
ccache c++ -std=c++17 -O2 -Ibuild/scratch/before-include -I"$boost" "$out/extra-timing.cpp" -o build/scratch/st-extra-before
build/scratch/st-extra-before > "$out/extra-timing-before.txt"
