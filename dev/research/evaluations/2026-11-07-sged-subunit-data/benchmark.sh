#!/usr/bin/env bash
# Run through /tmp/umvl-cpu/run on the workstation, or simbox-run.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch/baseline
source_dir=dev/research/evaluations/2026-11-07-sged-subunit-data
cp -a univariateML-cpp/include build/scratch/baseline/
cp "$source_dir/baseline_mle.hpp" build/scratch/baseline/include/cppunivariate/distributions/sged/mle.hpp
cp "$source_dir/baseline_certificate.hpp" build/scratch/baseline/include/cppunivariate/distributions/sged/certificate.hpp
boost_dir=$(Rscript -e 'cat(system.file("include",package="BH"))')
for version in before after; do
  include_dir=univariateML-cpp/include
  if [ "$version" = before ]; then include_dir=build/scratch/baseline/include; fi
  g++ -O2 -std=c++17 -I"$boost_dir" -I"$include_dir" "$source_dir/benchmark.cpp" -o "build/scratch/bench-$version"
  "build/scratch/bench-$version" > "$source_dir/timings-$version.txt"
done
python3 "$source_dir/oracle.py" "$source_dir/timings-after.txt" > "$source_dir/oracle.json"
g++ --version | head -1
