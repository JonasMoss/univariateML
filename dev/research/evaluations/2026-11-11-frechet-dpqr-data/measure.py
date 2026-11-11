"""Reproduce exact-binary64 comparisons against 100-digit mpmath.
Run from repository root: /tmp/umvl-cpu/run python3 <this file>.
Baseline headers are reconstructed with git show, never archived.
"""
import json, math, pathlib, subprocess
import mpmath as mp
ROOT = pathlib.Path(__file__).resolve().parents[4]
OUT = pathlib.Path(__file__).resolve().parent
SCRATCH = ROOT/'build/scratch/frechet-measure'
SCRATCH.mkdir(parents=True, exist_ok=True)
BASE = '6642c02b'
INC = ROOT/'univariateML-cpp/include'
REL = 'cppunivariate/distributions'
mp.mp.dps = 100
for family in ('frechet','invweibull'):
    s=subprocess.check_output(['git','show',f'{BASE}:univariateML-cpp/include/{REL}/{family}/dpqr.hpp'],cwd=ROOT,text=True)
    s=s.replace('"../../special/tails.hpp"',f'"{INC}/cppunivariate/special/tails.hpp"')
    s=s.replace('"../invweibull/dpqr.hpp"','"invweibull.hpp"')
    (SCRATCH/f'{family}.hpp').write_text(s)
SOURCE = r'''
#include HEADER
#include <iostream>
#include <iomanip>
int main() {
 int family,mode,lower,log; double x,a,m,s;
 std::cout<<std::setprecision(17);
 while(std::cin>>family>>mode>>x>>a>>m>>s>>lower>>log) {
  double v;
  if(mode==0) v=family==0 ? cppunivariate::distributions::dfrechet(x,a,m,s,log) : cppunivariate::distributions::dinvweibull(x,a,s,log);
  else if(mode==1) v=family==0 ? cppunivariate::distributions::pfrechet(x,a,m,s,lower,log) : cppunivariate::distributions::pinvweibull(x,a,s,lower,log);
  else v=family==0 ? cppunivariate::distributions::qfrechet(x,a,m,s,lower,log) : cppunivariate::distributions::qinvweibull(x,a,s,lower,log);
  std::cout<<v<<'\n';
 }
}
'''
(SCRATCH/'eval.cpp').write_text(SOURCE)
for name,header in [('baseline',str(SCRATCH/'frechet.hpp')),('current',str(INC/REL/'frechet/dpqr.hpp'))]:
    subprocess.run(['g++','-O2','-std=c++17',f'-DHEADER="{header}"',str(SCRATCH/'eval.cpp'),'-o',str(SCRATCH/name)],check=True)
rows=[]; refs=[]; labels=[]
def add(family,mode,x,a,m,s,lower,log,ref):
    rows.append(f'{family} {mode} {x:.17g} {a:.17g} {m:.17g} {s:.17g} {int(lower)} {int(log)}')
    refs.append(ref); labels.append(f'{family}:{mode}:{int(lower)}:{int(log)}')
for family in (0,1):
 for a in (.2,1.,3.,1e6,1e9,1e12,1e15):
  params=[(-a,a),(-a+.25,a),(-a+2.,a),(.1,1.),(1.,a)] if family==0 else [(0.,1.),(0.,a),(0.,.1)]
  for m,s in params:
   A,M,S=map(mp.mpf,(a,m,s))
   center=float(M+S)
   for offset in (-5.,-2.,-.25,0.,.5,2.,10.,36.,100.,700.):
    x=float(M+S*mp.exp(mp.mpf(offset)/A))
    if not math.isfinite(x) or x<=m: continue
    X=mp.mpf(x); z=mp.log((X-M)/S); h=mp.exp(-A*z)
    ld=mp.log(A/S)-(A+1)*z-h
    ll=-h; lu=mp.log(-mp.expm1(ll))
    for log in (False,True):
     add(family,0,x,a,m,s,True,log,ld if log else mp.exp(ld))
     for lower in (False,True):
      lp=ll if lower else lu
      add(family,1,x,a,m,s,lower,log,lp if log else mp.exp(lp))
   for lower in (False,True):
    for log,inputs in [(False,(1e-300,1e-12,.01,.2,math.exp(-1),1-math.exp(-1),.5,.9,math.nextafter(1.,0.))),(True,(-10000.,-800.,-36.,-1.,-.4586751453870819,-.1,-1e-12,-1e-300))]:
     for p in inputs:
      P=mp.mpf(p); lp=P if log else mp.log(P)
      # For lp<-250 the omitted log-hazard correction is <2e-109;
      # all grid shapes are >=.2. Avoid rounding 1-exp(lp) to one at 100 dps.
      lh=mp.log(-lp) if lower else lp if lp < -250 else mp.log(-mp.log(-mp.expm1(lp)))
      q=M+S*mp.exp(-lh/A)
      add(family,2,p,a,m,s,lower,log,q)
# A multiplier exp(1450) overflows, while the exact scaled quantile is normal.
# Log-hazard=log(p_upper)+O(exp(log(p_upper))); the omitted correction is <1e-600.
for family in (0,1):
 s=math.ulp(0.)
 add(family,2,-1450.,1.,0.,s,False,True,mp.mpf(s)*mp.exp(1450))
# Finite distance with an overflowing virtual subtraction in TwoDiff.
for a in (1.,1e15):
 x=-math.ldexp(3.,970); m=-float.fromhex('0x1.fffffffffffffp+1023'); s=1e308
 X,A,M,S=map(mp.mpf,(x,a,m,s)); z=mp.log((X-M)/S); h=mp.exp(-A*z)
 ld=mp.log(A/S)-(A+1)*z-h; ll=-h; lu=mp.log(-mp.expm1(ll))
 add(0,0,x,a,m,s,True,True,ld)
 for lower,lp in ((True,ll),(False,lu)):
  add(0,1,x,a,m,s,lower,True,lp)
# Deliberately cancel a nonzero center. These expose output conditioning,
# separately from the ordinary grid and the zero-center Gumbel seam.
for a in (1e6, 1e9, 1e12, 1e15):
 for center in (.25, 2.):
  m=-a+center; s=a
  A,M,S=map(mp.mpf,(a,m,s))
  p=float(-((-M)/S)**(-A))
  q=M+S*mp.exp(-mp.log(-mp.mpf(p))/A)
  add(0,2,p,a,m,s,True,True,q)
  labels[-1]='0:2:conditioned_zero'
payload='\n'.join(rows)+'\n'
summary={'baseline_commit':BASE,'digits':100,'rows':len(rows),'metrics':{}}
for version in ('baseline','current'):
 values=subprocess.check_output([str(SCRATCH/version)],input=payload,text=True).splitlines()
 assert len(values)==len(refs)
 metrics={}
 for row,label,ref,value in zip(rows,labels,refs,values):
  val=float(value)
  # Exclude underflowed/overflowed references from relative-error maxima.
  if math.isnan(val): raise AssertionError(row)
  rf=float(ref)
  if not math.isfinite(rf):
   if val!=rf: raise AssertionError((row,val,rf))
   continue
  if ref!=0 and abs(ref)<mp.mpf(2.2250738585072014e-308): continue
  err=abs(mp.mpf(val)-ref)/(abs(ref) if ref else 1)
  record=metrics.setdefault(label,{'max_relative':0.,'row':None,'ours':None,'reference':None})
  if err>record['max_relative']:
   record.update(max_relative=float(err),row=row,ours=value,reference=mp.nstr(ref,25))
 summary['metrics'][version]=metrics
(OUT/'measurements.json').write_text(json.dumps(summary,indent=2,allow_nan=False)+'\n')
print(json.dumps(summary,indent=2))
