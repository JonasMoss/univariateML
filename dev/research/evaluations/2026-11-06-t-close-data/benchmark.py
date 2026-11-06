"""Paired before/after workloads; one warmup, median of three measured calls."""
import json,subprocess,statistics,sys
from pathlib import Path
here=Path(__file__).resolve().parent
cases=json.loads((here.parent/'2026-11-04-t-cert-data/inputs.json').read_text())
normal=next(c for c in cases if c['name']=='normal-0')
final='--final' in sys.argv
rows=[r for r in json.loads((here/'benchmark.json').read_text()) if r['version']=='before'] if final else []
for case in [normal,dict(name='quantile-4000',x=[])]:
 inputs=str(len(case['x']))+' '+' '.join(format(x,'.17g') for x in case['x'])+'\n'
 for budget in [40,128,4096]:
  for version in (['after'] if final else ['before','after']):
   binary='build/scratch/t-close-before' if version=='before' else ('build/scratch/t-close-final-replay' if final else 'build/scratch/t-close-replay')
   runs=[]
   for repeat in range(4):
    lines=subprocess.check_output([binary,str(budget)],input=inputs,text=True).splitlines()
    runs.append([line.split() for line in lines])
   for j,family in enumerate(['lst','std']):
    results=[r[j] for r in runs[1:]]
    row=dict(name=case['name'],family=family,budget=budget,version=version,status=int(results[0][0]),profiles=int(results[0][1]),seconds_median=statistics.median(float(r[2]) for r in results))
    assert all(r[:2]==results[0][:2] for r in results)
    rows.append(row);print(row,flush=True)
(here/('benchmark-final.json' if final else 'benchmark.json')).write_text(json.dumps(rows,indent=2)+'\n')
