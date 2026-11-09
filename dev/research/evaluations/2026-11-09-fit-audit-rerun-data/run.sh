#!/usr/bin/env bash
# Exact frozen-job replay, resuming fit rows only on the recorded production tree.
set -euo pipefail
cd "$(dirname "$0")/../../../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp" PYTHONDONTWRITEBYTECODE=1
unset REFERENCE_FILE AUDIT_SHARDS AUDIT_SHARD
mkdir -p "$TMPDIR" build/scratch/fit-audit
here=dev/research/evaluations/2026-11-09-fit-audit-rerun-data
[ "$(git rev-parse HEAD)" = a99ca0dc92043120455401108cee5eeda17cafc4 ] || { echo 'This recorded job targets a99ca0dc. Use a new evidence directory for another tree.' >&2; exit 1; }
bash "$here/build_harness.sh"
AUDIT_BASELINE_CACHE_ONLY=1 /tmp/umvl-cpu/run python3 -u - <<'PY'
import pathlib,subprocess,sys
h=pathlib.Path('dev/research/evaluations/2026-11-09-fit-audit-rerun-data')
for name,args in [('audit.py',['fit']),('retry_fit.py',[]),('cache.py',[]),('check_reference.py',[]),('audit.py',['reference']),('retry_references.py',[])]:
 subprocess.run([sys.executable,str(h/name),*args],check=True)
PY
REFERENCE_SECONDS=20 /tmp/umvl-cpu/run python3 -u - <<'PY'
import pathlib,subprocess,sys
h=pathlib.Path('dev/research/evaluations/2026-11-09-fit-audit-rerun-data')
for name,args in [('audit.py',['reference']),('check_findings.py',[]),('contract.py',[]),('validate.py',[])]:
 subprocess.run([sys.executable,str(h/name),*args],check=True)
PY
set +e
/tmp/umvl-cpu/simbox-run just cpp-test > build/scratch/fit-audit/cpp-test.log 2>&1
rc=$?
set -e
if [ "$rc" = 75 ]; then
 /tmp/umvl-cpu/run4 just cpp-test > build/scratch/fit-audit/cpp-test.log 2>&1
elif [ "$rc" != 0 ]; then
 cat build/scratch/fit-audit/cpp-test.log; exit "$rc"
fi
/tmp/umvl-cpu/run python3 "$here/report_current.py"
