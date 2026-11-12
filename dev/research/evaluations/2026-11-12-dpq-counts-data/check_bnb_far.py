"""Independent gamma-identity references for late near-Poisson mass repair.
Run with /tmp/umvl-cpu/run; no frozen truth values are changed.
"""
import json, subprocess, math, hashlib
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent; ROOT=HERE.parents[3]
mp.mp.dps=500
r,a,b=map(mp.mpf,['1e8','1e16','1e8'])
rows=[]
for x in [10000.,10001.,1e6,1e10,1e100]:
 y=mp.mpf(x)
 ref=mp.loggamma(r+y)+mp.loggamma(a+r)+mp.loggamma(a+b)+mp.loggamma(b+y)-mp.loggamma(r)-mp.loggamma(a)-mp.loggamma(b)-mp.loggamma(a+r+b+y)-mp.loggamma(y+1)
 for mode,name in [('native','probe'),('double','probe-double')]:
  line=f'bnbinom 0 {x} 1 1 3 1e8 1e16 1e8\n'
  before=ROOT/'build/scratch/dpq-counts'/name
  after=ROOT/'build/scratch/dpq-counts-bnbinom'/name
  old=float(subprocess.check_output([str(before)],input=line,text=True))
  new=float(subprocess.check_output([str(after)],input=line,text=True))
  error=abs(mp.mpf(new)-ref)/max(1,abs(ref))
  assert math.isfinite(new) and error<mp.mpf('1e-10'),(mode,x,new,ref,error)
  rows.append(dict(mode=mode,x=x,before=old,after=new,reference=mp.nstr(ref,80),normalized_error=mp.nstr(error,30),binary_sha256=hashlib.sha256(after.read_bytes()).hexdigest()))
js=HERE/'cmp-far-js.json'
if js.exists():
 for x,new in zip([10000.,1e6],json.loads(js.read_text())['bnbinom']):
  ref=mp.mpf(next(row['reference']for row in rows if row['x']==x))
  error=abs(mp.mpf(new)-ref)/abs(ref)
  assert math.isfinite(new) and error<mp.mpf('1e-10')
  rows.append(dict(mode='JS',x=x,after=new,reference=mp.nstr(ref,80),normalized_error=mp.nstr(error,30)))
(HERE/'bnb-far-reference.json').write_text(json.dumps(rows,indent=2,allow_nan=False)+'\n')
print('near-Poisson mass references:',len(rows),'passed')
