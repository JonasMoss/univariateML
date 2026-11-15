#!/bin/sh
# Reproduce the TASK-220 evidence from the repository root (single-threaded
# steps; wrap each in /tmp/umvl-cpu/run on the shared workstation).
set -e
d=dev/research/evaluations/2026-11-14-sged-tail-data
boost=${BOOST_INCLUDE:-$HOME/R/x86_64-pc-linux-gnu-library/4.6/BH/include}
sh $d/build.sh baseline            # evaluator on master 2c62400a headers
sh $d/build.sh current             # evaluator on the working tree

# Prototype for the open GED item (not applied to the tree): pged routed to
# its three-word path when the double log radius is too sensitive.
rm -rf build/sged-tail/ged-proto && mkdir -p build/sged-tail/ged-proto/univariateML-cpp
cp -r univariateML-cpp/include build/sged-tail/ged-proto/univariateML-cpp/
(cd build/sged-tail/ged-proto && patch -p1 < ../../../$d/pged-prototype.patch)
g++ -std=c++17 -O2 -DSGED_TAIL_CURRENT -I build/sged-tail/ged-proto/univariateML-cpp/include -I "$boost" \
  $d/evaluate.cpp -o build/sged-tail/eval-ged-proto

cd $d
e=../../../../build/sged-tail
python3 sweep.py points --binary $e/eval-baseline --out reference.json.gz
for b in baseline current; do
  python3 sweep.py check --binary $e/eval-$b --reference reference.json.gz --out result-$b.json.gz
  python3 sweep.py replay --binary $e/eval-$b --out replay-$b.json
done
python3 sweep.py check --binary $e/eval-ged-proto --reference reference.json.gz --out result-pged-prototype.json.gz
python3 evaluator.py --binary $e/eval-baseline --op g --out evaluator-baseline.json.gz
python3 evaluator.py --binary $e/eval-current --op u --out evaluator-current.json.gz
python3 margin.py --baseline $e/eval-baseline --current $e/eval-current --out margin.json
cd ../../../.. && python3 $d/edge.py > $d/edge.txt
