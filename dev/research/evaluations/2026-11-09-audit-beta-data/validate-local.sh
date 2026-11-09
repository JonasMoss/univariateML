#!/usr/bin/env bash
# Invoke from repository root: /tmp/umvl-cpu/run4 bash dev/research/evaluations/2026-11-09-audit-beta-data/validate-local.sh
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export GOMAXPROCS=2 UV_THREADPOOL_SIZE=1
mkdir -p "$TMPDIR" build/scratch
{
  c++ --version
  Rscript -e 'cat("R ",as.character(getRversion()),"; BH ",as.character(packageVersion("BH")),"\n",sep="")'
  python3 -c 'import sys,mpmath; print("Python",sys.version.split()[0],"mpmath",mpmath.__version__)'
} > build/scratch/environment.log
run_check() {
  local check_name="$1"
  shift
  set +e
  "$@" > "build/scratch/$check_name.log" 2>&1
  local check_rc=$?
  set -e
  tail -n 25 "build/scratch/$check_name.log"
  echo "$check_name exit $check_rc"
  if [ "$check_rc" -ne 0 ]; then exit "$check_rc"; fi
}
run_check r-dev-final just test
run_check cpp-dev-final just cpp-test
run_check cpp-full env UMVL_TEST_TIER=full just cpp-test
run_check cpp-no-long-double just cpp-test-no-long-double
run_check r-full just test-full
run_check site-check just site-check
