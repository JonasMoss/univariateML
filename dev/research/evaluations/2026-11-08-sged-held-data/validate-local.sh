#!/bin/bash
# Run through /tmp/umvl-cpu/run4, never under another slot reservation.
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p build/tmp build/scratch
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
data=dev/research/evaluations/2026-11-08-sged-held-data
python3 - <<'PY'
from pathlib import Path
import re,subprocess
p=Path('univariateML-cpp/include/cppunivariate/distributions/sged/mle.hpp')
s=subprocess.check_output(['git','show','ddb399172bd99091c8d1b04dbf7fe329c8adc43e:'+str(p)],text=True)
s=re.sub(r'#include "([^"]+)"',lambda m:'#include "'+str((p.parent/m[1]).resolve())+'"',s)
Path('build/scratch/sged-baseline.hpp').write_text(s)
PY
for version in before after; do
  flags=()
  if [[ $version == before ]]; then flags=(-DSGED_BASELINE); fi
  c++ -O2 -std=c++17 "${flags[@]}" -Ibuild/scratch -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" "$data/timings.cpp" -o "build/scratch/timing-$version"
  "build/scratch/timing-$version" > "$data/small-$version.tsv"
done
c++ -O2 -std=c++17 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" "$data/probe.cpp" -o build/scratch/sged-after
build/scratch/sged-after > "$data/after-workstation.tsv"
if [[ ${SGED_SKIP_NO_LONG_DOUBLE:-0} != 1 ]]; then
  /usr/bin/time -f 'LOCAL_CPP_NO_LONG_DOUBLE_SECONDS=%e' just cpp-test-no-long-double
fi
sged_snapshot=dev/research/evaluations/2026-11-02-viz-clip/after-mobile-held-face.png
sged_snapshot_backup=build/scratch/site-snapshot-original.png
cp -p "$sged_snapshot" "$sged_snapshot_backup"
trap 'cp -p "$sged_snapshot_backup" "$sged_snapshot"' EXIT
/usr/bin/time -f 'SITE_SECONDS=%e' just site-check
