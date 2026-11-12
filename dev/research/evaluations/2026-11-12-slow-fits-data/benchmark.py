"""Run through run after compiling replay.cpp; timings are observations only."""
import gzip,json,subprocess,sys
from pathlib import Path
p=Path(__file__).resolve().parent;root=p.parents[3]
cases=json.load(gzip.open(root/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz','rt'))
rows=[]
for repetition in range(3):
 for index in (837,915,997):
  c=cases[index];h=c['held'];line=' '.join(map(str,[c['family'],len(c['data']),*c['data'],*[h.get(k,'nan') for k in ('size','prob','nu','shape','rate')]]))+'\n'
  r=subprocess.run([sys.argv[1]],input=line,text=True,capture_output=True,check=True)
  rows.append(dict(index=index,repetition=repetition,result=r.stdout.strip()))
  print(rows[-1],flush=True)
(p/'benchmark.json').write_text(json.dumps(rows,indent=2)+'\n')
