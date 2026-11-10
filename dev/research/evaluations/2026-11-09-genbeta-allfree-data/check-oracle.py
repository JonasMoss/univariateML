"""Run from repository root through /tmp/umvl-cpu/run; no package installs."""
import json,subprocess,math
from pathlib import Path
import mpmath as mp
mp.mp.dps=90
root=Path.cwd(); data=root/'dev/research/evaluations/2026-11-09-genbeta-allfree-data'
scratch=root/'build/scratch'
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
exe=scratch/'allfree-oracle'
flags=['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if '--double' in __import__('sys').argv else []
subprocess.run(['c++','-O2','-std=c++17',*flags,'-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(data/'oracle.cpp'),'-o',str(exe)],check=True)
lines=subprocess.check_output([str(exe)],text=True).splitlines()
ys=[[mp.mpf(z) for z in row] for row in [('.1','.3','1','2.6'),('.5','1.5'),('.01','.1','.5','3.39')]]
checks=[]; conditional={}
for line in lines:
 row=line.split(); tag=row.pop(0)
 if tag=='I':
  h,q,ok,lo,hi=map(mp.mpf,row); assert ok==1
  for hs in (h*mp.mpf('.9'),h*mp.mpf('1.1')):
   for qs in (q*mp.mpf('.8'),q*mp.mpf('1.2')):
    assert mp.digamma(lo+qs)-mp.digamma(lo)>hs
    assert mp.digamma(hi+qs)-mp.digamma(hi)<hs
  checks.append({'type':'interval','h':float(h),'q':float(q),'passed':True})
 elif tag=='C':
  sample=int(row[0]);h,ok,pl,ph,ql,qh,lower,upper=map(mp.mpf,row[1:]); assert ok==1,line
  y=ys[sample];s=h*sum(y)/len(y);r=-sum(mp.log(-mp.expm1(-h*v)) for v in y)/len(y)
  def score(lp,lq):
   p,q=mp.exp(lp),mp.exp(lq)
   return mp.digamma(p+q)-mp.digamma(p)-s,mp.digamma(p+q)-mp.digamma(q)-r
  lp,lq=mp.findroot(score,(mp.log((pl+ph)/2),mp.log((ql+qh)/2)),tol=mp.mpf('1e-75'),maxsteps=300)
  p,q=mp.exp(lp),mp.exp(lq)
  value=len(y)*(mp.log(h)-p*s-(q-1)*r-mp.log(mp.beta(p,q)))
  assert pl<=p<=ph and ql<=q<=qh,line
  assert lower<=value<=upper,line
  conditional[sample,str(h)]=(p,q,value)
  checks.append({'type':'conditional','sample':sample,'h':float(h),'p':str(p),'q':str(q),'loglik':str(value),'upper_gap':float(upper-value),'passed':True})
 elif tag=='B':
  sample=int(row[0]);h,ok,upper=map(mp.mpf,row[1:]);assert ok==1
  assert upper>=conditional[sample,str(h)][2]
  checks.append({'type':'nonstationary_tangent','sample':sample,'h':float(h),'passed':True})
 elif tag=='S':
  sample=int(row[0]);gu,domain,coef=map(mp.mpf,row[1:]);y=ys[sample]
  assert domain>0 and coef>0
  mu=sum(y)/len(y);c=mp.log(mu)-sum(mp.log(v) for v in y)/len(y)
  k=mp.findroot(lambda k:mp.log(k)-mp.digamma(k)-c,(1/(2*c),1/c))
  for h in (domain/100,domain/10,domain/2,domain):
   ss=h*mu;r=-sum(mp.log(-mp.expm1(-h*v)) for v in y)/len(y)
   def score(lp,lq):
    p,q=mp.exp(lp),mp.exp(lq)
    return mp.digamma(p+q)-mp.digamma(p)-ss,mp.digamma(p+q)-mp.digamma(q)-r
   lp,lq=mp.findroot(score,(mp.log(k/ss),mp.log(k)),tol=mp.mpf('1e-65'),maxsteps=300)
   p,q=mp.exp(lp),mp.exp(lq)
   value=len(y)*(mp.log(h)-p*ss-(q-1)*r-mp.log(mp.beta(p,q)))
   assert value<=gu+len(y)*coef*h*h
  checks.append({'type':'quadratic_small_power','sample':sample,'domain':float(domain),'coefficient':float(coef),'passed':True})
 elif tag=='Q':
  sample=int(row[0]);ql,qh,upper=map(mp.mpf,row[1:]);y=ys[sample]
  assert 0<ql<1<qh
  for q in (ql,(ql+1)/2,mp.mpf(1),(1+qh)/2,qh):
   for h in (mp.mpf('.00001'),mp.mpf('.01'),mp.mpf(1),mp.mpf(10),mp.mpf(10000)):
    ss=h*sum(y)/len(y);r=-sum(mp.log(-mp.expm1(-h*v)) for v in y)/len(y)
    lo=mp.mpf(0);hi=max(1,2*q/ss)
    for _ in range(320):
     p=(lo+hi)/2
     if mp.digamma(p+q)-mp.digamma(p)>ss:lo=p
     else:hi=p
    p=(lo+hi)/2
    value=len(y)*(mp.log(h)-p*ss-(q-1)*r-mp.log(mp.beta(p,q)))
    assert value<=upper
  checks.append({'type':'ridge_strip','sample':sample,'ql':float(ql),'qh':float(qh),'upper':float(upper),'passed':True})
 elif tag=='T':
  sample=int(row[0]);L,small,H,large=map(mp.mpf,row[1:]);y=ys[sample]
  for h in (L,L/2,L/100,H,2*H,10*H):
   s=h*sum(y)/len(y);r=-sum(mp.log(-mp.expm1(-h*v)) for v in y)/len(y)
   # Analytic shape rectangle checks the stated envelope against feasible
   # positive coefficients, including exponentially growing q at large h.
   for q in (mp.mpf('.01'),mp.mpf('1'),mp.mpf('100'),mp.exp(h*min(y))):
    for p in (1/h,mp.mpf('.01')/h,mp.mpf('10')/h):
     val=len(y)*(mp.log(h)-p*s-(q-1)*r-mp.log(mp.beta(p,q)))
     assert val<=(small if h<=L else large),line
  checks.append({'type':'tails','sample':sample,'small_cutoff':float(L),'small_upper':float(small),'large_cutoff':float(H),'large_upper':float(large),'passed':True})
result={'precision':mp.mp.dps,'compiler':subprocess.check_output(['c++','--version'],text=True).splitlines()[0],'double_width':bool(flags),'checks':checks}
name='oracle-double.json' if flags else 'oracle.json'
(data/name).write_text(json.dumps(result,indent=2)+'\n')
print(f'{len(checks)} checks passed; {name}')
