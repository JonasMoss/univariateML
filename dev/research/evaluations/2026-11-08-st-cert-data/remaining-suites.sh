#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
out=dev/research/evaluations/2026-11-08-st-cert-data
rc=0
/tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 bash "$out/final-r.sh" > build/scratch/final-r-remote.txt 2>&1 || rc=$?
if [ "$rc" -eq 75 ]; then
 /tmp/umvl-cpu/run4 bash "$out/final-r.sh"
 printf 'r-dev workstation fallback\nr-full workstation fallback\n' > "$out/r-machines.txt"
elif [ "$rc" -ne 0 ]; then
 exit "$rc"
else
 rsync -a -e 'ssh -F /home/jonas/.ssh/config -o BatchMode=yes' "simbox:umvl-wt/st-cert/$out/r-*.txt" "$out/"
 printf 'r-dev simbox\nr-full simbox\n' > "$out/r-machines.txt"
fi
bash "$out/final-local.sh"
