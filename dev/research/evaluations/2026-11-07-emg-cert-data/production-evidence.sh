#!/usr/bin/env bash
# Invoke under a workstation CPU slot, or through simbox-run (not both).
set -euo pipefail
evidence=dev/research/evaluations/2026-11-07-emg-cert-data
snapshot=build/scratch/emg-baseline/include/cppunivariate
mkdir -p "$snapshot/distributions/emg" build/tmp
cp "$evidence"/original-emg/*.hpp "$snapshot/distributions/emg/"
for name in core special; do ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/$name" "$snapshot/$name"; done
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
flags=(-std=c++17 -O2 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost")
c++ -I"$snapshot/.." "${flags[@]}" "$evidence/check.cpp" -o build/scratch/emg-original
c++ "${flags[@]}" "$evidence/check.cpp" -o build/scratch/emg-after
python3 "$evidence/bench.py" build/scratch/emg-original build/scratch/emg-after
UMVL_BENCH_DEFAULT=1 python3 "$evidence/bench.py" build/scratch/emg-original build/scratch/emg-after
for mode in native double; do
 extra=(); suffix=""
 if [ "$mode" = double ]; then extra=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS); suffix=-double; fi
 for kind in location fixed-shape outer; do
   c++ "${flags[@]}" "${extra[@]}" "$evidence/$kind-oracle.cpp" -o "build/scratch/emg-$kind$suffix"
   "build/scratch/emg-$kind$suffix" > "$evidence/$kind$suffix.csv"
 done
 if [ "$mode" = double ]; then
   python3 "$evidence/oracle.py" --double
   python3 "$evidence/outer-oracle.py" --double
 else
   python3 "$evidence/oracle.py"
   python3 "$evidence/outer-oracle.py"
 fi
done
