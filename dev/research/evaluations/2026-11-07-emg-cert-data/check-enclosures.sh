#!/usr/bin/env bash
# Invoke through the workstation CPU wrapper or simbox-run.
set -euo pipefail
cd "$(dirname "$0")/../../../.."
evidence=dev/research/evaluations/2026-11-07-emg-cert-data
flags=(-std=c++17 -O2 -IunivariateML-cpp/include -IunivariateML-cpp/tests)
if [ -n "${EMG_BOOST_INCLUDE:-}" ]; then flags+=(-I"$EMG_BOOST_INCLUDE"); fi
for mode in native double; do
  extra=(); suffix=""
  if [ "$mode" = double ]; then extra=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS); suffix=-double; fi
  c++ "${flags[@]}" "${extra[@]}" "$evidence/location-oracle.cpp" -o "build/scratch/emg-location$suffix"
  "build/scratch/emg-location$suffix" > "$evidence/location$suffix.csv"
  c++ "${flags[@]}" "${extra[@]}" "$evidence/fixed-shape-oracle.cpp" -o "build/scratch/emg-shape$suffix"
  "build/scratch/emg-shape$suffix" > "$evidence/fixed-shape$suffix.csv"
  if [ "$mode" = double ]; then python3 "$evidence/oracle.py" --double; else python3 "$evidence/oracle.py"; fi
done
c++ "${flags[@]}" "$evidence/global-probe.cpp" -o build/scratch/emg-global-probe
build/scratch/emg-global-probe > "$evidence/global-probe.csv"
python3 - <<'PY'
import json
from pathlib import Path
p=Path('dev/research/evaluations/2026-11-07-emg-cert-data')
names=['global-probe.csv']
for suffix in ['', '-double']:
    names += [f'location{suffix}.csv',f'fixed-shape{suffix}.csv',f'oracle{suffix}.json',f'fixed-shape-oracle{suffix}.json']
print('EVIDENCE='+json.dumps({name:(p/name).read_text() for name in names}))
PY
