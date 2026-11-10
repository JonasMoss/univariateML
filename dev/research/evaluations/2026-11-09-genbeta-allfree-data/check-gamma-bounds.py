"""Independent signed fourth-order neighborhood oracle in both widths."""
import json,shutil,subprocess,sys
from pathlib import Path
import mpmath as mp
mp.mp.dps=90
root=Path.cwd();data=root/'dev/research/evaluations/2026-11-09-genbeta-allfree-data';scratch=root/'build/scratch'
double='--double' in sys.argv
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
exe=scratch/'gamma-bounds-oracle'
subprocess.run([*(['ccache','c++'] if shutil.which('ccache') else ['c++']),'-O2','-std=c++17',*(['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if double else []),'-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(data/'gamma-oracle.cpp'),'-o',str(exe)],check=True)
samples=[[mp.mpf(z) for z in row] for row in [('.1','.3','1','2.6'),('.5','1.5'),('.01','.1','.5','3.39'),('.2','.6','1.1','2.1')]]
records=[]
for line in subprocess.check_output([str(exe)],text=True).splitlines():
 sample,ok,H,c2,C4,kl,kh,ql,qh,nl,nh=map(mp.mpf,line.split());sample=int(sample)
 y=samples[sample];mu=sum(y)/len(y);v=[z/mu for z in y];c=-sum(mp.log(z) for z in v)/len(v)
 k=mp.findroot(lambda q:mp.log(q)-mp.digamma(q)-c,(kl,kh),tol=mp.mpf('1e-80'))
 assert kl<=k<=kh
 actual=(k-1)*(sum(z*z for z in v)/len(v)-(k+1)/k)/24
 assert actual<=c2
 row=dict(sample=sample,enclosed=bool(ok),c2_upper=float(c2),c2=str(actual),passed=True)
 if ok:
  assert c2+C4*H*H<0
  G=k*mp.log(k)-k-mp.loggamma(k)-(k-1)*c
  for h in [H/100,H/10,H/2,H]:
   r=-sum(mp.log(-mp.expm1(-h*z)) for z in v)/len(v)
   def score(lp,lq):
    p,q=mp.exp(lp),mp.exp(lq)
    return mp.digamma(p+q)-mp.digamma(p)-h,mp.digamma(p+q)-mp.digamma(q)-r
   lp,lq=mp.findroot(score,(mp.log(k/h-(k-1)/2),mp.log(k)),tol=mp.mpf('1e-75'),maxsteps=100)
   p,q=mp.exp(lp),mp.exp(lq)
   F=mp.log(h)-p*h-(q-1)*r-mp.log(mp.beta(p,q))
   assert ql<=q<=qh
   assert nl<=h*(p+(q-1)/2)<=nh
   assert F<=G+c2*h*h+C4*h**4
  row.update(cutoff=float(H),fourth_upper=float(C4),profile_points=4)
 else:assert actual>0
 records.append(row)
out=dict(precision=90,double_width=double,checks=records)
(data/('gamma-bounds-double.json' if double else 'gamma-bounds.json')).write_text(json.dumps(out,indent=2)+'\n')
print('4 signed-coefficient and 8 profile-neighborhood checks passed', 'double' if double else 'native')
