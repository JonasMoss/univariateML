#!/usr/bin/env bash
# Run from the worktree root through /tmp/umvl-cpu/run. No nested slot lock.
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p build/tmp build/scratch/slow-fits-baseline
study=dev/research/evaluations/2026-11-12-slow-fits-data
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
# Rebuild, rather than retaining an archived source tree as evidence.
git archive c4ed89f6a4222e133c7524cf726fea4cb9369593 univariateML-cpp/include | tar -x -C build/scratch/slow-fits-baseline
c++ -std=c++17 -O2 -Ibuild/scratch/slow-fits-baseline/univariateML-cpp/include -I"$boost" "$study/replay.cpp" -o build/scratch/replay-baseline
python3 "$study/replay.py" build/scratch/replay-baseline build/scratch/replay-baseline.jsonl
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" "$study/replay.cpp" -o build/scratch/replay-current
python3 "$study/replay.py" build/scratch/replay-current build/scratch/replay-current.jsonl
c++ -std=c++17 -O1 -pg -Ibuild/scratch/slow-fits-baseline/univariateML-cpp/include -I"$boost" "$study/profile.cpp" -o build/scratch/profile-baseline
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" "$study/precision-score.cpp" -o build/scratch/precision-score
python3 "$study/precision-check.py" build/scratch/precision-score build/scratch/precision-check.json
cd build/scratch
for index in 837 915 997; do
  ./profile-baseline "$index"
  gprof profile-baseline gmon.out > "profile-rebuilt-$index.txt"
done
