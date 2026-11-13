#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
root=$PWD/dev/research/evaluations/2026-11-13-qtie-data
/tmp/umvl-cpu/run bash "$root/prepare-baseline.sh"
boost=$(/tmp/umvl-cpu/run Rscript -e 'cat(system.file("include",package="BH"))')
flags=(-std=c++17 -O2 -DDOCTEST_CONFIG_IMPLEMENT "-DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\"" -IunivariateML-cpp/tests -I"$boost")
for mode in before after; do
 include=univariateML-cpp/include
 extra=()
 if [[ $mode == before ]]; then include=build/scratch/baseline/univariateML-cpp/include; extra=(-include build/scratch/baseline-shim.hpp); fi
 for probe in roundtrip samplers; do
  /tmp/umvl-cpu/run c++ "${flags[@]}" "${extra[@]}" -I"$include" "$root/$probe.cpp" -o "build/scratch/$probe-$mode"
  /tmp/umvl-cpu/run "build/scratch/$probe-$mode" > "$root/$probe-$mode.csv"
 done
done

/tmp/umvl-cpu/run cmp "$root/samplers-before.csv" "$root/samplers-after.csv"
