#!/usr/bin/env python3
import csv, math, subprocess
from pathlib import Path
p=Path(__file__).resolve().parent
rows=list(csv.DictReader((p/'samples.csv').open()))
cl=list(csv.DictReader((p/'classification.csv').open()))
ix=[i for i,r in enumerate(cl) if r['screen_winner']=='tail increase']
lines=[]
for i in ix:
 r=rows[i];x=r['data'].split()
 for n in range(int(r['Nmax'])+1,1001):lines.append(f"{len(x)} {n} "+' '.join(x))
print('extended profiles',len(lines),flush=True)
out=subprocess.run(['build/scratch/bbinom-map-check'],input='\n'.join(lines)+'\n',capture_output=True,text=True,check=True).stdout.splitlines()
assert len(out)==len(lines)
result=[]; pos=0
for i in ix:
 r=rows[i]; n=1000-int(r['Nmax']); block=[v.split() for v in out[pos:pos+n]];pos+=n
 valid=[v for v in block if int(v[1]) in (0,3,4,5,6) and math.isfinite(float(v[2]))];best=max(valid,key=lambda v:float(v[2]))
 half=next(v for v in block if v[0]=='500');end=block[-1]
 result.append(dict(index=i,N=best[0],status=best[1],loglik=best[2],alpha=best[3],beta=best[4],failed=n-len(valid),tail_half=half[2],tail_end=end[2],tail_half_status=half[1],tail_end_status=end[1],Nmax=1000))
with (p/'extended.csv').open('w',newline='') as f:
 w=csv.DictWriter(f,fieldnames=result[0]);w.writeheader();w.writerows(result)
print('extended samples',len(ix),'failures',sum(int(r['failed']) for r in result),flush=True)
