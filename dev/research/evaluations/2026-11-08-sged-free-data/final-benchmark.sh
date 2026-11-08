#!/bin/bash
# The before binary must be compiled from the initial source before editing.
# Run through /tmp/umvl-cpu/run after the four-slot workstation suite exits.
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
boost_include=$(Rscript -e 'cat(system.file("include",package="BH"))')
g++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost_include" dev/research/evaluations/2026-11-08-sged-free-data/benchmark.cpp -o build/scratch/benchmark-after
python3 - <<'PY'
import csv,io,json,statistics,subprocess
from pathlib import Path
p=Path('dev/research/evaluations/2026-11-08-sged-free-data')
p.joinpath('balanced.txt').write_text('-2\n-2\n3\n3\n')
raw={}
for stage in ['before','after']:
 for sample in ['small','golden','balanced']:
  runs=[]
  for _ in range(3):
   text=subprocess.check_output(['build/scratch/benchmark-'+stage],input=p.joinpath(sample+'.txt').read_bytes()).decode()
   runs.append(list(csv.DictReader(io.StringIO(text))))
  rows=[]
  for i,row in enumerate(runs[0]):
   for repeat in runs[1:]:
    assert all(repeat[i][k]==row[k] for k in row if k!='ms'),(stage,sample,i)
   rows.append({**row,'ms':statistics.median(float(repeat[i]['ms']) for repeat in runs)})
  with p.joinpath(stage+'-'+sample+'.csv').open('w') as f:
   writer=csv.DictWriter(f,fieldnames=list(rows[0]));writer.writeheader();writer.writerows(rows)
  raw[stage+'-'+sample]=runs
p.joinpath('timings-raw.json').write_text(json.dumps(raw,indent=2)+'\n')
print('3 sequential fits per row; 32 subsets x 3 samples x 2 stages; medians written.')
PY
python3 dev/research/evaluations/2026-11-08-sged-free-data/check_math.py
