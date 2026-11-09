#!/bin/sh
# Invoke through run: native and double-width family checks run sequentially.
set -eu
export TMPDIR="$PWD/build/tmp"
evidence=dev/research/evaluations/2026-11-08-bnbinom-allfree-data
python3 "$evidence/audit.py"
sh dev/research/evaluations/2026-11-08-bnbinom-joint-data/targeted.sh > "$evidence/native-targeted.txt"
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -Wall -Wextra -Wpedantic -DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS \
  -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$bh" \
  -DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\" \
  build/scratch/bnbinom-joint/main.cpp univariateML-cpp/include/cppunivariate/distributions/bnbinom/test.cpp \
  -o build/scratch/bnbinom-allfree/tests-no-long-double
build/scratch/bnbinom-allfree/tests-no-long-double > "$evidence/no-long-double-targeted.txt"
cat "$evidence/native-targeted.txt" "$evidence/no-long-double-targeted.txt"
python3 "$evidence/audit.py"
