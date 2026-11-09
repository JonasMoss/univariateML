import json
from pathlib import Path
p=Path(__file__).resolve().parent
summary=[]
for n in [30,101,1000]:
 before=[line.split() for line in (p/f'before-{n}.log').read_text().splitlines()]
 after=[line.split() for line in (p/f'after-{n}.log').read_text().splitlines()]
 assert len(before)==len(after)=={30:352,101:264,1000:88}[n]
 for route in range(4):
  rows=[(a,b) for a,b in zip(before,after) if int(a[2])==route]
  for a,b in rows:assert a[:3]==b[:3] and a[3]==b[3]=='0',(a,b)
  differences=[float(b[7])-float(a[7]) for a,b in rows]
  assert min(differences)>=-1e-7,(n,route,min(differences))
  summary.append(dict(n=n,route=route,fits=len(rows),same=sum(abs(d)<=1e-7 for d in differences),better=sum(d>1e-7 for d in differences),worst=min(differences),best=max(differences),before_seconds=sum(float(a[8]) for a,b in rows),after_seconds=sum(float(b[8]) for a,b in rows)))
(p/'comparison-summary.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
