#!/usr/bin/env bash
# Invoke only after targeted validation has passed and source is frozen.
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
out=dev/research/evaluations/2026-11-08-st-cert-data
rc=0
/tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 bash "$out/remote-final-commands.sh" > "$out/remote-final-attempt.txt" 2>&1 || rc=$?
if [ "$rc" -eq 75 ]; then
 printf 'cpp-dev workstation fallback\ncpp-full workstation fallback\ncpp-no-long-double workstation\nr-dev workstation fallback\nr-full workstation fallback\n' > "$out/final-machines.txt"
 /tmp/umvl-cpu/run4 bash "$out/native-final-commands.sh"
elif [ "$rc" -ne 0 ]; then
 exit "$rc"
else
 for file in cpp-dev.txt.gz cpp-full.txt.gz cpp-no-long-double-simbox.txt.gz r-dev.txt.gz r-full.txt.gz; do
  rsync -a -e 'ssh -F /home/jonas/.ssh/config -o BatchMode=yes' "simbox:umvl-wt/st-cert/$out/$file" "$out/"
 done
 printf 'cpp-dev simbox\ncpp-full simbox\ncpp-no-long-double simbox\nr-dev simbox\nr-full simbox\n' > "$out/final-machines.txt"
 /tmp/umvl-cpu/run4 just cpp-test-no-long-double > "$out/cpp-no-long-double-workstation.txt.gz" 2>&1
fi
/tmp/umvl-cpu/run bash -c '
 set -euo pipefail
 cpu=$(python3 -c "import os; print(min(os.sched_getaffinity(0)))")
 exec taskset -c "$cpu" bash dev/research/evaluations/2026-11-08-st-cert-data/js-final-commands.sh
'
# Retry the required GCC 13 replay after releasing every workstation slot.
# A dropped host uses the already completed current-source workstation run.
rc=0
/tmp/umvl-cpu/simbox-run just cpp-test-no-long-double > "$out/cpp-no-long-double-simbox-final-attempt.txt" 2>&1 || rc=$?
if [ "$rc" -eq 75 ]; then
 printf 'cpp-no-long-double simbox final retry unreachable (75); current-source workstation fallback above\n' >> "$out/final-machines.txt"
elif [ "$rc" -ne 0 ]; then
 exit "$rc"
else
 printf 'cpp-no-long-double simbox final retry passed\n' >> "$out/final-machines.txt"
fi
