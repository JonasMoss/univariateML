import json,subprocess,sys
from pathlib import Path
here=Path(__file__).resolve().parent
old=here.parent/'2026-11-06-t-port-data'
cases=json.loads((here.parent/'2026-11-04-t-cert-data/inputs.json').read_text())
mm=json.loads((here.parent/'2026-11-04-t-closure-data/multimodality.json').read_text())
for name,c in mm.items():cases.append(dict(name=name,x=[float(x) for x,w in zip(c['x'],c['weights']) for _ in range(int(w))]))
cases.append(dict(name='quantile-4000',x=[]))
rows=[]
for c in cases:
 s=str(len(c['x']))+' '+' '.join(format(x,'.17g') for x in c['x'])+'\n'
 lines=subprocess.check_output([sys.argv[1] if len(sys.argv)>1 else 'build/scratch/t-close-final-replay'],input=s,text=True).splitlines()
 for family,line in zip(['lst','std'],lines):
  f=line.split();r=dict(name=c['name'],family=family,status=int(f[0]),profiles=int(f[1]),seconds=float(f[2]),values=f[3:6],box=f[6:12],unresolved=f[12:])
  rows.append(r);print(r,flush=True)
(here/'replay.json').write_text(json.dumps(rows,indent=2)+'\n')
oldrows=json.loads((old/'replay.json').read_text())
labels=['finite','cauchy','normal','t2','collapse','unbounded','budget','tie','numerical_failure','invalid_data']
for a,b in zip(oldrows,rows):
 if a['outcome']!='budget':assert a['outcome']==labels[b['status']],(a['name'],a['family'],a['outcome'],b)
print('Preserved all 60 prior certified outcomes')
