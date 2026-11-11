"""A physical fatigue quantile can survive underflow of alpha*z."""
import json,sys
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
sys.path.insert(0,str(HERE));import audit
mp.mp.dps=200
jobs=[];expected=[]
for a in (float.fromhex('0x0.0000000000001p-1022'),1e-308):
 for lower in (True,False):
  p=mp.exp(mp.mpf(-.6931471805599453));z=mp.sqrt(2)*mp.erfinv(2*p-1)*(1 if lower else -1)
  q=mp.mpf(1e308)*mp.expm1(2*mp.asinh(mp.mpf(a)*z/2))
  jobs.append(('fatigue',2,-.6931471805599453,lower,True,[a,1e308,-1e308]));expected.append(float(q))
rows=[dict(job=j,actual=a,expected=b,error=audit.error(a,b,True)) for j,a,b in zip(jobs,audit.evaluate(jobs),expected)]
(HERE/'fatigue-underflow.json').write_text(json.dumps(rows,indent=2)+'\n')
print('fatigue inverse underflow',len(rows),'worst',max(r['error'] for r in rows),flush=True)
assert all(r['error']<=1e-9 for r in rows)
r=rows[0];p=ROOT/'univariateML-cpp/include/cppunivariate/distributions/fatigue/test.cpp';title='fatigue: physical inverse survives latent underflow'
if title not in p.read_text():p.write_text(p.read_text()+'\nTEST_CASE("'+title+'") {\n CHECK(cppunivariate::distributions::qfatigue(-.6931471805599453,4.9406564584124654e-324,1e308,-1e308,true,true)==doctest::Approx('+repr(r['expected'])+').epsilon(1e-9).scale(0));\n}\n')
