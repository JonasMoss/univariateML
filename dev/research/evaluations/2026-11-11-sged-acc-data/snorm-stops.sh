#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp
D=dev/research/evaluations/2026-11-11-sged-acc-data
python3 - <<'PY'
from pathlib import Path
D=Path('dev/research/evaluations/2026-11-11-sged-acc-data')
(D/'snorm-stops-inputs.txt').write_text('\n'.join(l for l in (D/'inputs.txt').read_text().splitlines() if int(l.split()[0]) in [6579,6583,6587,6589,6593])+'\n')
PY
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" "$D/snorm-stops.cpp" -o build/scratch/snorm-stops
build/scratch/snorm-stops "$D/snorm-stops-inputs.txt" > "$D/snorm-stops-budget.txt"
