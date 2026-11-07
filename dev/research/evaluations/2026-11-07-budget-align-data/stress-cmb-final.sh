#!/bin/bash
set -eu
base=dev/research/evaluations/2026-11-07-budget-align-data
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -Ibindings -IunivariateML-cpp/tests -IunivariateML-cpp/include -I"$boost" "$base/stress.cpp" -o build/scratch/budget-stress-final-cmb
python3 - <<'META'
from pathlib import Path
import hashlib,json,subprocess
paths=sorted([*Path('univariateML-cpp/include').rglob('*.hpp'),*Path('bindings/common').rglob('*.hpp')])
manifest=''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+str(p)+'\n' for p in paths)
print(json.dumps(dict(source_headers_sha256=hashlib.sha256(manifest.encode()).hexdigest(),probe_sha256=hashlib.sha256(Path('dev/research/evaluations/2026-11-07-budget-align-data/stress.cpp').read_bytes()).hexdigest(),machine='simbox',compiler=subprocess.check_output(['c++','--version'],text=True).splitlines()[0],seed=143,n=100000,timeout_seconds=60,cmb_timeout_seconds=180)),flush=True)
META
STRESS_FAMILIES=cmb STRESS_SECONDS=60 STRESS_CMB_SECONDS=180 STRESS_BINARY=build/scratch/budget-stress-final-cmb python3 "$base/stress-run.py"
