#!/usr/bin/env python3
"""A finite symmetric Student member refutes the failed-restart half faces."""
from pathlib import Path
import json,math
import mpmath as mp
x=[-2.,-1.,-.4,0.,.2,.3,.5,1.,2.,5.]
# The core evaluates this feasible member in its centered coordinates.
origin=(min(x)+max(x))/2
mean=0.
for a in x:mean+=(a-origin)/len(x)
mu=origin+mean
records=[]
def ll(dps):
 with mp.workdps(dps):
  nu=mp.mpf(4);unit=mp.sqrt(mp.mpf('.5'))
  normalizer=mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(mp.pi*nu)/2
  finite=mp.fsum(normalizer-mp.log(unit)-(nu+1)/2*mp.log1p(((mp.mpf(a)-mu)/unit)**2/nu) for a in x)
  faces=[]
  for side,endpoint in [(1,min(x)),(-1,max(x))]:
   face=mp.fsum(mp.log(2)+normalizer-(nu+1)/2*mp.log1p((side*(mp.mpf(a)-endpoint))**2/nu) for a in x)
   faces.append((side,endpoint,finite,face,finite-face))
  return faces
first=ll(100);second=ll(160)
for a,b in zip(first,second):
 stable=all(abs(u-v)<mp.mpf('1e-80') for u,v in zip(a[2:],b[2:]))
 records.append(dict(side=b[0],endpoint=b[1],finite_mean=mu,finite_sd=1,nu=4,finite_loglik=mp.nstr(b[2],80),face_loglik=mp.nstr(b[3],80),gain=mp.nstr(b[4],80),stable=bool(stable),passed=bool(stable and b[4]>0)))
(Path(__file__).parent/'sstd-failed-face-refutations.json').write_text(json.dumps(records,indent=2)+'\n')
print(records)
assert all(r['passed'] for r in records)
