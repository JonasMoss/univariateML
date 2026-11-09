#!/bin/sh
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
for family in ${TARGET_FAMILIES:-ged lst}; do
  c++ ${TARGET_CXXFLAGS:-} -std=c++17 -O0 -DDOCTEST_CONFIG_IMPLEMENT_WITH_MAIN '-DCPPUNIV_DISTRIBUTIONS_DIR="univariateML-cpp/include/cppunivariate/distributions"' -IunivariateML-cpp/tests -IunivariateML-cpp/include -I"$boost" "univariateML-cpp/include/cppunivariate/distributions/$family/test.cpp" -o "build/scratch/$family-targeted"
  "build/scratch/$family-targeted" --test-case='*extreme*,*huge shapes*,*huge-shape*,*tiny gamma*,*unrepresentable gamma shape*,*scaled fluctuations*,*unsupported log-scale*,*uniform-edge*,*transformed uniform edges*,*subnormal gamma draws*,*overflowing standardization*,*density square overflow*'
done
