"""Independent 80-digit checks of the q-eliminated and beta cell envelopes."""
import json,subprocess,sys
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
root=Path.cwd();data=root/'dev/research/evaluations/2026-11-09-genbeta-allfree-data'
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
flags=['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if '--double' in sys.argv else []
exe=root/'build/scratch/power-oracle'
subprocess.run(['c++','-O2','-std=c++17',*flags,'-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(data/'power-oracle.cpp'),'-o',str(exe)],check=True)
ys=[[mp.mpf(z) for z in row] for row in [('.1','.3','1','2.6'),('.5','1.5'),('.01','.1','.5','3.39')]]
records=[];gamma_count=0;beta_count=0
for line in subprocess.check_output([str(exe)],text=True).splitlines():
 row=line.split();sample=int(row[0]);L,H,gu,bu,pl,ph,ql,qh=map(mp.mpf,row[1:]);v=ys[sample];n=len(v)
 def rr(h):return -sum(mp.log1p(-mp.exp(-h*w)) for w in v)/n
 rL,rH=rr(L),rr(H)
 if mp.isfinite(gu):
  A=L+mp.log(rH)-rL;assert A>0 and rL<1,line
  lo,hi=1/(2*A),1/A
  for _ in range(250):
   p=(lo+hi)/2
   if mp.log(p)-mp.digamma(p)>A:lo=p
   else:hi=p
  p=(lo+hi)/2
  exact=mp.log(H)+rL+p*mp.log(p)-p-mp.loggamma(p)-A*p
  assert exact<=gu,line
  gamma_count+=1
 if mp.isfinite(bu):
  assert pl>0 and ql>0
  for h in (L,(L+H)/2,H):
   r=rr(h)
   def scores(lp,lq):
    p,q=mp.exp(lp),mp.exp(lq)
    return mp.digamma(p+q)-mp.digamma(p)-h,mp.digamma(p+q)-mp.digamma(q)-r
   lp,lq=mp.findroot(scores,(mp.log((pl+ph)/2),mp.log((ql+qh)/2)),maxsteps=300,tol=mp.mpf('1e-65'))
   p,q=mp.exp(lp),mp.exp(lq)
   value=mp.log(h)-p*h-(q-1)*r-mp.log(mp.beta(p,q))
   assert value<=bu,line
   if mp.isfinite(gu):assert value<=gu,line
   beta_count+=1
 records.append({'sample':sample,'left':float(L),'right':float(H),'gamma_available':bool(mp.isfinite(gu)),'beta_available':bool(mp.isfinite(bu)),'passed':True})
assert gamma_count>=30 and beta_count>=30
name='power-double.json' if flags else 'power.json'
(data/name).write_text(json.dumps({'precision':mp.mp.dps,'double_width':bool(flags),'gamma_cell_checks':gamma_count,'beta_profile_points':beta_count,'cells':records},indent=2)+'\n')
print(f'{gamma_count} q-eliminated envelopes; {beta_count} beta-profile points passed')
