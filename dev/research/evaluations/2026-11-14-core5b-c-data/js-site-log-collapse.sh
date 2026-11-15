#!/bin/bash
set -eu
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CCACHE_DIR="${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}" CCACHE_BASEDIR="$PWD" CCACHE_NOHASHDIR=1 CCACHE_MAXSIZE=8G
export LC_ALL=C.UTF-8 TZ=UTC
D=dev/research/evaluations/2026-11-14-core5b-c-data
python3 - <<'ADD_CASE'
from pathlib import Path
p=Path('bindings/js/stop-test.mjs');s=p.read_text()
needle="    [()=>api.gpd.fit([0,1],{mu:0,xi:1}),'nomle','unattained']"
row="    [()=>api.gb2.fit([1e300,1e300*(1+Number.EPSILON)],{shape2:2,scale:1e300}),'numerical','evaluation']"
if row not in s:
 assert needle in s
 p.write_text(s.replace(needle,needle+',\n'+row))
ADD_CASE
just js-build > "$D/js-final-build.log" 2>&1
just js-test > "$D/js-final-test.log" 2>&1
just site-check > "$D/site-final-check.log" 2>&1
