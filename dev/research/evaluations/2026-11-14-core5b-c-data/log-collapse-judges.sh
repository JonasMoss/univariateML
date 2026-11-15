#!/bin/bash
set -eu
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
D=dev/research/evaluations/2026-11-14-core5b-c-data
F=betapr,burr,cmp,dnorm,emg,foldnorm,gb2,gengamma,gpd,hnorm,hyper,invgamma,invpareto,kumar,lnorm,lomax,nhyper,pareto,pareto4,rayleigh,skellam,snorm,std,triang,yulesimon
B=dev/research/evaluations/2026-11-14-fit-snapshot-data
for script in fit-judge fit-density fit-consistency fit-refine; do
  extra=(); if [ "$script" = fit-refine ]; then extra=(--cache-only); fi
  /tmp/umvl-cpu/run python3 "dev/validation/$script.py" build/snapshots/native build/snapshots/double "${extra[@]}" --prune > "$D/$script.log" 2>&1
done
/tmp/umvl-cpu/run python3 "$D/prune-unrepresentable-face.py" > "$D/pruned-unrepresentable-face.log"
/tmp/umvl-cpu/run python3 "$D/retain-excluded.py" > "$D/retained-excluded.log"
/tmp/umvl-cpu/run just strict build/snapshots/native build/snapshots/double > "$D/strict.log" 2>&1

/tmp/umvl-cpu/run python3 "$D/known-diff.py" > "$D/known-diff.log"
