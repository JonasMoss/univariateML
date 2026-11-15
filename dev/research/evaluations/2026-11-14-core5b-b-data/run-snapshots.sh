#!/usr/bin/env bash
# Invoke from this worktree through /tmp/umvl-cpu/run (one slot).
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
F=beta,bnbinom,cmb,dlaplace,dweibull,fatigue,frechet,genbeta,gompertz,hcauchy,huber,invexp,invparalogis,invweibull,llogis,logitnorm,naka,paralogis,pareto3,power,sgomp,sn,st,trgamma,weibull
D=dev/research/evaluations/2026-11-14-core5b-b-data
python3 dev/validation/fit-snapshot.py --output build/snapshots/native --family "$F" --jobs 1 > "$D/native-snapshot.log" 2>&1
python3 dev/validation/fit-snapshot.py --output build/snapshots/double --family "$F" --jobs 1 --no-long-double > "$D/double-snapshot.log" 2>&1
