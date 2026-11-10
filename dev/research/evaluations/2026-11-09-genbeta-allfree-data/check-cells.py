"""Independent shape/score/curvature enclosure checks at 80 decimal digits."""
import json,subprocess,sys
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
root=Path.cwd();data=root/'dev/research/evaluations/2026-11-09-genbeta-allfree-data'
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
flags=['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if '--double' in sys.argv else []
exe=root/'build/scratch/cell-oracle'
subprocess.run(['c++','-O2','-std=c++17',*flags,'-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(data/'cell-oracle.cpp'),'-o',str(exe)],check=True)
ys=[[mp.mpf(z) for z in row] for row in [('.1','.3','1','2.6'),('.5','1.5'),('.01','.1','.5','3.39')]]
records=[]
for line in subprocess.check_output([str(exe)],text=True).splitlines():
 row=line.split();sample=int(row[0]);L,H,ok,pl,ph,ql,qh,slo,shi,curv=map(mp.mpf,row[1:])
 if not ok:
  records.append({'sample':sample,'left':float(L),'right':float(H),'available':False,'requires_subdivision':True})
  continue
 v=ys[sample];n=len(v)
 for h in (L,(L+H)/2,H):
  r=-sum(mp.log(-mp.expm1(-h*w)) for w in v)/n
  D=sum(w/mp.expm1(h*w) for w in v)/n
  K=sum(w*w*mp.exp(-h*w)/(-mp.expm1(-h*w))**2 for w in v)/n
  def score(lp,lq):
   p,q=mp.exp(lp),mp.exp(lq)
   return mp.digamma(p+q)-mp.digamma(p)-h,mp.digamma(p+q)-mp.digamma(q)-r
  lp,lq=mp.findroot(score,(mp.log((pl+ph)/2),mp.log((ql+qh)/2)),maxsteps=300,tol=mp.mpf('1e-65'))
  p,q=mp.exp(lp),mp.exp(lq)
  T=mp.polygamma(1,p+q);A=mp.polygamma(1,p)-T;B=mp.polygamma(1,q)-T
  S=1/h-p+(q-1)*D
  J=-1/h**2-(q-1)*K+(B-2*T*D+A*D**2)/(A*B-T*T)
  assert pl<=p<=ph and ql<=q<=qh,line
  assert slo<=S<=shi,line
  assert J<=curv,line
 records.append({'sample':sample,'left':float(L),'right':float(H),'available':True,'curvature_upper':float(curv) if mp.isfinite(curv) else None,'passed':True})
assert sum(bool(r.get('passed')) for r in records)>=30
name='cells-double.json' if flags else 'cells.json'
(data/name).write_text(json.dumps({'precision':mp.mp.dps,'double_width':bool(flags),'cells':records,'comparisons':sum(bool(r.get('passed')) for r in records)*3},indent=2)+'\n')
print(f'{len(records)} cells; {sum(bool(r.get("passed")) for r in records)*3} high-precision points passed')
