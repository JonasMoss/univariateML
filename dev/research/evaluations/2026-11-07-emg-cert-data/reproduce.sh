#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch/emg-baseline/include/cppunivariate/distributions/emg
baseline=c118a003f111b0d145f8f8b9385ee0a87c1fd4cd
evidence=dev/research/evaluations/2026-11-07-emg-cert-data
snapshot=build/scratch/emg-baseline/include/cppunivariate
for name in mle.hpp dpqr.hpp logconcave_profile.hpp; do
  git show "$baseline:univariateML-cpp/include/cppunivariate/distributions/emg/$name" > "$snapshot/distributions/emg/$name"
done
for name in core special; do ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/$name" "$snapshot/$name"; done
boost=$(/tmp/umvl-cpu/run Rscript -e 'cat(system.file("include",package="BH"))')
flags=(-std=c++17 -O2 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost")
/tmp/umvl-cpu/run c++ -I"$snapshot/.." "${flags[@]}" "$evidence/check.cpp" -o build/scratch/emg-original
/tmp/umvl-cpu/run c++ "${flags[@]}" "$evidence/check.cpp" -o build/scratch/emg-after
/tmp/umvl-cpu/run python3 "$evidence/bench.py" build/scratch/emg-original build/scratch/emg-after
for mode in native double; do
  extra=(); suffix=""
  if [ "$mode" = double ]; then extra=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS); suffix=-double; fi
  /tmp/umvl-cpu/run c++ "${flags[@]}" "${extra[@]}" "$evidence/location-oracle.cpp" -o "build/scratch/emg-location$suffix"
  /tmp/umvl-cpu/run "build/scratch/emg-location$suffix" > "$evidence/location$suffix.csv"
  /tmp/umvl-cpu/run c++ "${flags[@]}" "${extra[@]}" "$evidence/fixed-shape-oracle.cpp" -o "build/scratch/emg-shape$suffix"
  /tmp/umvl-cpu/run "build/scratch/emg-shape$suffix" > "$evidence/fixed-shape$suffix.csv"
  if [ "$mode" = double ]; then
    /tmp/umvl-cpu/run python3 "$evidence/oracle.py" --double
  else
    /tmp/umvl-cpu/run python3 "$evidence/oracle.py"
  fi
done
/tmp/umvl-cpu/run python3 "$evidence/profile_faces.py"
if [ "${EMG_GLOBAL_PROBE:-0}" = 1 ]; then
  /tmp/umvl-cpu/run c++ "${flags[@]}" "$evidence/global-probe.cpp" -o build/scratch/emg-global-probe
  /tmp/umvl-cpu/run build/scratch/emg-global-probe > "$evidence/global-probe.csv"
fi
