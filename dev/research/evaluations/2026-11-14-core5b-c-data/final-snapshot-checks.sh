#!/bin/bash
set -eu
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
D=dev/research/evaluations/2026-11-14-core5b-c-data
F=betapr,burr,cmp,dnorm,emg,foldnorm,gb2,gengamma,gpd,hnorm,hyper,invgamma,invpareto,kumar,lnorm,lomax,nhyper,pareto,pareto4,rayleigh,skellam,snorm,std,triang,yulesimon
B=dev/research/evaluations/2026-11-14-fit-snapshot-data
for mode in native double; do
  python3 dev/validation/fit-snapshot.py --diff "$B/$mode.jsonl.gz" "build/snapshots/$mode" --family "$F" > "$D/diff-$mode.log" || test "$?" = 1
  python3 dev/validation/fit-snapshot.py --outcomes "build/snapshots/$mode" > "$D/outcomes-$mode.log"
done
python3 "$D/verify-threshold-replay.py" > "$D/threshold-replay.log"
python3 "$D/adapter-audit.py" > "$D/adapter-audit.log"
python3 "$D/classify.py" > "$D/classification.log"
python3 "$D/high_precision.py" > "$D/high-precision.log"
