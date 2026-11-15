#!/bin/bash
set -eu
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CCACHE_DIR="${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}" CCACHE_BASEDIR="$PWD" CCACHE_NOHASHDIR=1 CCACHE_MAXSIZE=8G
export LC_ALL=C.UTF-8 TZ=UTC
D=dev/research/evaluations/2026-11-14-core5b-c-data
python3 dev/validation/fit-snapshot.py --output build/snapshots/log-collapse-native --family gb2 --jobs 1 > "$D/log-collapse-native-run.log" 2>&1
python3 dev/validation/fit-snapshot.py --output build/snapshots/log-collapse-double --family gb2 --jobs 1 --no-long-double > "$D/log-collapse-double-run.log" 2>&1
