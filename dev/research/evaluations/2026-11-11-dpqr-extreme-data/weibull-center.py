"""Exact input inverse witnesses at the unit-hazard CDF/survival."""
import math,json,sys
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(HERE));import audit
mp.mp.dps=160
constants={}
for n,v in [('cdf',-mp.expm1(-1)),('survival',mp.exp(-1)),('log_cdf',mp.log(-mp.expm1(-1)))]:
 hi=float(v);lo=float(v-mp.mpf(hi));constants[n]=dict(high=hi,low=lo,value=mp.nstr(v,100))
jobs=[];expected=[]
for k in (1e-8,1e-12,1e-16):
 for lower,logp,p in [(True,False,constants['cdf']['high']),(False,False,constants['survival']['high']),(True,True,constants['log_cdf']['high'])]:
  pp=mp.exp(mp.mpf(p)) if logp else mp.mpf(p)
  h=-mp.log1p(-pp) if lower else -mp.log(pp)
  jobs.append(('weibull',2,p,lower,logp,[k,1.]));expected.append(float(mp.exp(mp.log(h)/mp.mpf(k))))
rows=[dict(job=j,actual=a,expected=b,error=audit.error(a,b,True)) for j,a,b in zip(jobs,audit.evaluate(jobs),expected)]
(HERE/('weibull-center-'+sys.argv[1]+'.json')).write_text(json.dumps(dict(constants=constants,rows=rows),indent=2)+'\n')
print(constants)
print('Weibull center',sys.argv[1],'worst',max(r['error'] for r in rows))
