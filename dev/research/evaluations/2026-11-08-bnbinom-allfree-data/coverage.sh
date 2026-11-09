#!/bin/sh
# Invoke through simbox-run, or through run locally after exit 75.
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch/bnbinom-allfree/coverage
evidence=dev/research/evaluations/2026-11-08-bnbinom-allfree-data
python3 "$evidence/audit.py"
python3 - <<'PY'
from pathlib import Path
import csv,json
p=Path('build/scratch/bnbinom-allfree/coverage/source.json')
source=Path('dev/research/evaluations/2026-11-08-bnbinom-allfree-data/source.json').read_bytes()
if p.exists():
    assert p.read_bytes()==source,'Cannot resume coverage after changing sources'
else:
    p.write_bytes(source)
# Retain only complete CSV/sample pairs after an interrupted write. This
# repairs the output transaction, without discarding any completed outcome.
directory=p.parent
c=directory/'coverage.csv';j=directory/'samples.jsonl'
if c.exists():
    rows=list(csv.reader(c.open()))
    observed={}
    if j.exists():
        for line in j.read_text().splitlines():
            try:
                value=json.loads(line)
                observed[value['id']]=value
            except json.JSONDecodeError:
                pass
    kept=[]
    if rows:
        for row in rows[1:]:
            if len(row)==len(rows[0]) and row[0].isdigit() and int(row[0]) in observed:
                kept.append(row)
    if kept:
        with c.open('w') as stream:
            writer=csv.writer(stream);writer.writerow(rows[0]);writer.writerows(kept)
    else:
        c.unlink()
    j.write_text(''.join(json.dumps(observed[int(row[0])],separators=(',',':'))+'\n' for row in kept))
PY
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -Wall -Wextra -Wpedantic -IunivariateML-cpp/include -I"$bh" "$evidence/coverage.cpp" -o build/scratch/bnbinom-allfree/coverage/check
build/scratch/bnbinom-allfree/coverage/check "$1"
python3 "$evidence/audit.py"
