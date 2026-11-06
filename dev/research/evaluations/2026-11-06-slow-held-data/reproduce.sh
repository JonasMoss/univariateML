#!/usr/bin/env bash
# From the worktree root: /tmp/umvl-cpu/run bash <this file>
# No git mutations; reconstruct the three baseline headers with git show.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
mkdir -p "$TMPDIR" build/scratch
scratch=build/scratch
data=dev/research/evaluations/2026-11-06-slow-held-data
mkdir -p "$scratch/before-include"
cp -a univariateML-cpp/include/. "$scratch/before-include/"
for id in tlambda emg bhatt; do
 path="univariateML-cpp/include/cppunivariate/distributions/$id/mle.hpp"
 git show "f30584cd1819744e130850bcb1e58a5d1c52ed28:$path" > "$scratch/before-include/cppunivariate/distributions/$id/mle.hpp"
done
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
for binary in before after; do
 include=univariateML-cpp/include
 if [ "$binary" = before ]; then include="$scratch/before-include"; fi
 c++ -O2 -std=c++17 -I"$include" -I"$boost" dev/validation/fit-speed-comparison.cpp -o "$scratch/$binary"
 c++ -O2 -std=c++17 -I"$include" -I"$boost" "$data/decisions.cpp" -o "$scratch/decisions-$binary"
 "$scratch/decisions-$binary" > "$scratch/decisions-$binary.txt"
done
cmp "$scratch/decisions-before.txt" "$scratch/decisions-after.txt"
for rep in 1 2 3; do
 for binary in before after; do
  : > "$scratch/$binary-timings-$rep.txt"
  for id in tlambda emg bhatt std lst sstd sged slash fpareto gb2; do
   "$scratch/$binary" "$id" 0 >> "$scratch/$binary-timings-$rep.txt"
  done
 done
done
for id in tlambda emg bhatt; do
 for binary in before after; do
  "$scratch/$binary" "$id" 1 > "$scratch/$id-$binary-equivalence.txt"
 done
 python3 dev/validation/compare-fit-speed.py "$scratch/$id-before-equivalence.txt" "$scratch/$id-after-equivalence.txt" --differences "$scratch/$id-differences.csv"
done
