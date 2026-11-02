#!/usr/bin/env python3
import csv, math, subprocess
from pathlib import Path
p=Path(__file__).resolve().parent
rows=list(csv.DictReader((p/'samples.csv').open())); lines=[]; sizes=[]
for r in rows:
 x=r['data'].split(); m=int(r['M']); n=int(r['Nmax']); sizes.append(n-m+1)
 lines.extend(f"{len(x)} {k} "+' '.join(x) for k in range(m,n+1))
print('profiles:',len(lines),flush=True)
out=subprocess.run(['build/scratch/bbinom-map-check'],input='\n'.join(lines)+'\n',capture_output=True,text=True,check=True).stdout.splitlines()
assert len(out)==len(lines)
result=[]; pos=0
for i,(r,n) in enumerate(zip(rows,sizes)):
 block=out[pos:pos+n]; pos+=n; parsed=[v.split() for v in block]; valid=[v for v in parsed if int(v[1]) in (0,3,4,5,6) and math.isfinite(float(v[2]))]
 best=max(valid,key=lambda v:float(v[2])) if valid else ['','-1','nan','nan','nan']
 half=max(int(r['M']),int(r['Nmax'])//2)
 tail=next(v for v in parsed if int(v[0])==int(r['Nmax']))
 halfrow=next(v for v in parsed if int(v[0])==half)
 result.append(dict(index=i,N=best[0],status=best[1],loglik=best[2],alpha=best[3],beta=best[4],failed=n-len(valid),tail_half=halfrow[2],tail_end=tail[2],tail_half_status=halfrow[1],tail_end_status=tail[1]))
with (p/'certified.csv').open('w',newline='') as f:
 w=csv.DictWriter(f,fieldnames=result[0]);w.writeheader();w.writerows(result)
print('samples',len(result),'samples with failed fixed-size profiles',sum(int(r['failed'])>0 for r in result),flush=True)
