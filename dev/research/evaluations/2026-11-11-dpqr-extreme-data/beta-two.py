"""1800-digit independent beta-prime unit/two-shape tail witnesses."""
import json,math,sys
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(HERE));import audit
from quality.truth_reference import pair
if '--double' in sys.argv:audit.EXE=audit.WORK/'probe-double'
mp.mp.dps=1800
jobs=[];expected=[]
for a in (float.fromhex('0x0.0000000000001p-1022'),1e-300,1e-8,.5,1.,2.,1e8,1e308):
 for x in (float.fromhex('0x0.0000000000001p-1022'),1e-300,1e-100,1e-8,.1,1.,10.,1e8,1e100,1e300,1e308):
  aa,q=mp.mpf(a),mp.mpf(x);lx=mp.log(q)-mp.log1p(q);w=1/(1+q)
  for b in (1.,2.):
   lp=aa*lx+(mp.log1p(aa*w) if b==2 else 0);tails=pair(lp)
   for lower,ref in zip((True,False),tails):
    jobs.append(('betapr',1,x,lower,True,[a,b,1.]));expected.append(float(ref))
    if ref>mp.log(mp.mpf('1e-300')):
     jobs.append(('betapr',1,x,lower,False,[a,b,1.]));expected.append(float(mp.exp(ref)))
rows=[dict(job=j,actual=a,expected=b,error=audit.error(a,b,j[1]==1 and not j[4])) for j,a,b in zip(jobs,audit.evaluate(jobs),expected)]
label='beta-two-double' if '--double' in sys.argv else 'beta-two'
(HERE/(label+'.json')).write_text(json.dumps(rows,indent=2)+'\n')
print(label,len(rows),'checks; worst',max(r['error'] for r in rows),flush=True)
for r in rows:
 if r['error']>1e-10:print('FAILED',r,flush=True)
assert all(r['error']<=1e-10 for r in rows)
