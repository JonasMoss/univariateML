"""Independent 80-digit roots for the three positive-domain replay discrepancies."""
from pathlib import Path
from load_evidence import read_json
import json,collections
import mpmath as mp
mp.mp.dps=80
p=Path(__file__).resolve().parent
rows=read_json(p/'results.json');cases=read_json(p/'cases.json');out=[]
for row in rows:
 if row['family']!='zipf' or row['differences']!='coefficients':continue
 c=cases[row['case']-1];x=c['x'];N=row['old_estimates']['N'];target=mp.fsum(mp.log(v) for v in x)/len(x)
 def norm(s):return mp.fsum(mp.power(k,-s) for k in range(1,N+1)) if N<100 else mp.zeta(s)-mp.zeta(s,N+1)
 def score(s):return -mp.diff(norm,s)/norm(s)-target
 got=row['new_estimates']['s'];ref=mp.findroot(score,(mp.mpf(got)-mp.mpf('1e-5'),mp.mpf(got)+mp.mpf('1e-5')))
 err=abs(got-ref)/max(1,abs(ref));assert err<mp.mpf('1e-10'),(row['case'],err)
 out.append(dict(case=row['case'],N=N,old=row['old_estimates']['s'],new=got,reference=str(ref),scaled_error=float(err)))
(p/'zeta-replay-reference.json').write_text(json.dumps(out,indent=2)+'\n');print(len(out),'positive replay discrepancies independently verified')
