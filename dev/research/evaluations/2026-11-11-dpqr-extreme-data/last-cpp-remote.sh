#!/bin/bash
set -u
h=dev/research/evaluations/2026-11-11-dpqr-extreme-data
just cpp-test-no-long-double > build/scratch/remote-final/cpp-test-no-long-double.log 2>&1
code=$?
python3 - "$h/remote-final-runs.tsv" "$code" <<'PY'
import sys
from pathlib import Path
p=Path(sys.argv[1]);c=sys.argv[2];rows=p.read_text().splitlines();rows=[f'cpp-test-no-long-double\tsimbox\t{c}' if x.startswith('cpp-test-no-long-double\t') else x for x in rows];p.write_text('\n'.join(rows)+'\n')
PY
PYTHONPATH=dev/validation python3 -m unittest quality.test_truth_extreme > build/scratch/remote-final/generator-unit.log 2>&1
u=$?
Rscript "$h/sged-cusps-R.R"
python3 "$h/sged-cusps-R-reference.py"
tail -18 build/scratch/remote-final/cpp-test-no-long-double.log
cat build/scratch/remote-final/generator-unit.log
printf 'cpp=%s generator=%s\n' "$code" "$u"
if [ "$code" -ne 0 ] || [ "$u" -ne 0 ];then exit 1;fi
