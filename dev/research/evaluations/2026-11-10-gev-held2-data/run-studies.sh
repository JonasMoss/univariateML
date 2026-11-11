#!/usr/bin/env bash
set -euo pipefail
b=$(Rscript -e 'cat(system.file("include",package="BH"))')
d=dev/research/evaluations/2026-11-10-gev-held2-data
for name in study allfree; do
  g++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$b" "$d/$name.cpp" -o "build/scratch/$name-final"
  for maxit in 100 1000; do
    "build/scratch/$name-final" "$maxit" > "$d/$name-simbox-$maxit.csv"
  done
done
tar -C "$d" -cf - study-simbox-100.csv study-simbox-1000.csv allfree-simbox-100.csv allfree-simbox-1000.csv
