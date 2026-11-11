set -euo pipefail
export TMPDIR=$PWD/build/tmp
b=$(Rscript -e 'cat(system.file("include",package="BH"))')
d=dev/research/evaluations/2026-11-10-gev-held2-data
for source in allfree edges; do
  g++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$b" "$d/$source.cpp" -o "build/scratch/combination-$source"
  for maxit in 100 1000; do
    "build/scratch/combination-$source" "$maxit" > "$d/combination-$source-$maxit.csv"
  done
done
