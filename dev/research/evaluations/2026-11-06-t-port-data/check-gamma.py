"""Independent 80-digit checks of the native gamma enclosures."""
import json, subprocess
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
root=Path(__file__).resolve().parents[4]
rows=[]
for line in subprocess.check_output([str(root/'build/scratch/t-port-gamma')],text=True).splitlines():
    v,k,l,h=line.split();v=mp.mpf(float(v));k=int(k);l=mp.mpf(float(l));h=mp.mpf(float(h))
    if k==0:exact=mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(v/2)/2
    elif k==1:exact=(mp.digamma((v+1)/2)-mp.digamma(v/2))/2
    elif k==2:exact=(mp.polygamma(1,v/2)-mp.polygamma(1,(v+1)/2))/4
    else:exact=(mp.digamma((v+1)/2)-mp.digamma(v/2))/2-1/(2*v)
    rows.append(dict(v=str(v),order=k,lower=str(l),upper=str(h),reference=str(exact),passed=bool(l<=exact<=h)))
Path(__file__).with_name('gamma.json').write_text(json.dumps(rows,indent=2)+'\n')
assert all(r['passed'] for r in rows), [r for r in rows if not r['passed']]
print('Gamma enclosure checks passed:',len(rows))
