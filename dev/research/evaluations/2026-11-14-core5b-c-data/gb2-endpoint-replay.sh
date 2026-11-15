#!/bin/bash
set -eu
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
D=dev/research/evaluations/2026-11-14-core5b-c-data
python3 dev/validation/fit-snapshot.py --output build/snapshots/endpoint-native --family betapr,gb2,gpd --jobs 1 > "$D/gb2-endpoint-native-run.log" 2>&1
python3 dev/validation/fit-snapshot.py --output build/snapshots/endpoint-double --family betapr,gb2,gpd --jobs 1 --no-long-double > "$D/gb2-endpoint-double-run.log" 2>&1
