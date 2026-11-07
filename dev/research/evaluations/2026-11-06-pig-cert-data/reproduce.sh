#!/usr/bin/env bash
# Run at the worktree root through /tmp/umvl-cpu/run (one slot).
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch/pig-cert-before/cppunivariate/distributions/pig
pig_evidence_dir=dev/research/evaluations/2026-11-06-pig-cert-data
pig_before_dir=build/scratch/pig-cert-before/cppunivariate
pig_boost_include=${Boost_INCLUDE_DIR:-$(Rscript -e 'cat(system.file("include",package="BH"))')}
git show bb734cd78b846a565d8835606328bb1285a82db6:univariateML-cpp/include/cppunivariate/distributions/pig/mle.hpp > "$pig_before_dir/distributions/pig/mle.hpp"
ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/core" "$pig_before_dir/core"
ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/special" "$pig_before_dir/special"
ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/distributions/invgauss" "$pig_before_dir/distributions/invgauss"
ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/distributions/pig/dpqr.hpp" "$pig_before_dir/distributions/pig/dpqr.hpp"
python3 - <<'PY'
import json
from pathlib import Path
x=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/pig/golden.json').read_text())['data']
Path('build/scratch/pig-golden-data.hpp').write_text('std::vector<int> x{'+','.join(map(str,x))+'};\n')
PY
for pig_version in before after; do
  if [[ $pig_version == before ]]; then pig_include=build/scratch/pig-cert-before; else pig_include=univariateML-cpp/include; fi
  c++ -O2 -std=c++17 -I "$pig_include" -I "$pig_boost_include" "$pig_evidence_dir/benchmark.cpp" -o build/scratch/pig-benchmark
  build/scratch/pig-benchmark > "$pig_evidence_dir/timing-$pig_version.csv"
  c++ -O2 -std=c++17 -DPIG_GOLDEN -I build/scratch -I "$pig_include" -I "$pig_boost_include" "$pig_evidence_dir/benchmark.cpp" -o build/scratch/pig-benchmark
  build/scratch/pig-benchmark > "$pig_evidence_dir/timing-golden-$pig_version.csv"
done
python3 "$pig_evidence_dir/oracle.py"
c++ -O2 -std=c++17 -I univariateML-cpp/include -I "$pig_boost_include" "$pig_evidence_dir/check-oracle.cpp" -o build/scratch/pig-check-oracle
build/scratch/pig-check-oracle > "$pig_evidence_dir/oracle-cpp.csv"
python3 "$pig_evidence_dir/check-oracle.py" > "$pig_evidence_dir/oracle-check.json"
c++ -O2 -std=c++17 -I univariateML-cpp/include -I "$pig_boost_include" "$pig_evidence_dir/profile-search.cpp" -o build/scratch/pig-profile-search
build/scratch/pig-profile-search > "$pig_evidence_dir/profile-search.csv"
python3 "$pig_evidence_dir/check-proof.py"
python3 "$pig_evidence_dir/shape-oracle.py"
c++ -O2 -std=c++17 -I univariateML-cpp/include -I "$pig_boost_include" "$pig_evidence_dir/check-shape-oracle.cpp" -o build/scratch/pig-check-shape-oracle
build/scratch/pig-check-shape-oracle > "$pig_evidence_dir/shape-oracle-cpp.csv"
python3 "$pig_evidence_dir/check-shape-oracle.py" > "$pig_evidence_dir/shape-oracle-check.json"
