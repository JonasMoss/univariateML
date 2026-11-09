#!/bin/sh
# Invoke through /tmp/umvl-cpu/run. Reconstructs the frozen base read-only.
set -eu
record=dev/research/evaluations/2026-11-08-gb2-power-data
mkdir -p build/tmp build/scratch/gb2-power-baseline
{
 c++ --version | head -n 1
 Rscript -e 'cat(R.version.string, "\nBH ", as.character(packageVersion("BH")), "\nRcpp ", as.character(packageVersion("Rcpp")), "\n", sep="")'
} > "$record/environment-local.txt"
git archive ddb39917 univariateML-cpp/include | tar -x -C build/scratch/gb2-power-baseline
GB2_INCLUDE=build/scratch/gb2-power-baseline/univariateML-cpp/include sh "$record/check.sh" > "$record/before.txt"
sh "$record/check.sh" > "$record/after.txt"
python3 "$record/curvature.py" > "$record/curvature.json"
python3 "$record/high_precision.py" > "$record/high-precision.json"
python3 "$record/accuracy.py" > "$record/accuracy.json"
python3 "$record/free_scale_bound.py" > "$record/free-scale-bound.json"
