#!/usr/bin/env python3
"""Independent numerical maxima, exact binary64 inputs; no production imports."""
import gzip,json
from fractions import Fraction
from pathlib import Path
import mpmath as mp
root=Path(__file__).resolve().parents[4]
archive=json.load(gzip.open(root/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz','rt'))
cases={r['index']:r for r in archive}
results=[]
for digits in (100,160):
 mp.mp.dps=digits
 # Burr: solve in centered, standardized log data, never rounded log data.
 r=cases[636];x=list(map(mp.mpf,r['data']));k=mp.mpf(r['held']['shape1']);c0=mp.mpf(875134658462.2328);unit=x[2]
 u=[c0*mp.log(v/unit) for v in x]
 sigmoid=lambda z:1/(1+mp.exp(-z))
 def scores(q,d):
  w=[1-(1+k)*sigmoid(q*v-d) for v in u]
  return len(x)/q+mp.fsum(v*z for v,z in zip(u,w)),-mp.fsum(w)
 q,d=mp.findroot(scores,(1,.5),tol=mp.mpf(10)**(-digits+20))
 c=c0*q;s=unit*mp.exp(d/c)
 def ll(c,s):
  return mp.fsum(mp.log(k)+mp.log(c)-mp.log(v)-mp.log1p(mp.exp(-c*mp.log(v/s)))-k*mp.log1p(mp.exp(c*mp.log(v/s))) for v in x)
 maximum=ll(c,s);public=ll(c0,mp.mpf(2.042845496593945));nearest=ll(mp.mpf(float(c)),mp.mpf(float(s)))
 results.append(dict(family='burr',index=636,digits=digits,maximum=str(maximum),public_value=str(public),gap_relative=str((maximum-public)/max(1,abs(maximum))),nearest_value=str(nearest),nearest_gap_relative=str((maximum-nearest)/max(1,abs(maximum))),shape2=str(c),shape2_relative_error=str(abs(c0/c-1)),scale=str(s),score=[str(v) for v in scores(q,d)]))
 r=cases[2568];x=list(map(mp.mpf,r['data']));t=mp.mpf(r['held']['shape3']);n=len(x)
 sy=mp.fsum(t*mp.log(v) for v in x);sc=mp.fsum(mp.log(-mp.expm1(t*mp.log(v))) for v in x)
 def scores(a,b):return sy-n*(mp.digamma(a)-mp.digamma(a+b)),sc-n*(mp.digamma(b)-mp.digamma(a+b))
 a,b=mp.findroot(scores,(2017533,579011618),tol=mp.mpf(10)**(-digits+20))
 def ll(a,b):return n*(mp.log(t)-mp.loggamma(a)-mp.loggamma(b)+mp.loggamma(a+b))+a*sy+(b-1)*sc-mp.fsum(mp.log(v) for v in x)
 maximum=ll(a,b);public=ll(mp.mpf(2017533.8265625883),mp.mpf(579011618.7379755))
 results.append(dict(family='genbeta',index=2568,digits=digits,maximum=str(maximum),public_value=str(public),gap_relative=str((maximum-public)/max(1,abs(maximum))),shape1=str(a),shape2=str(b),score=[str(v) for v in scores(a,b)]))
 r=cases[457];x=list(map(mp.mpf,r['data']));mean=mp.fsum(x)/len(x)
 def ll(s,p):return mp.fsum(mp.loggamma(s+1)-mp.loggamma(v+1)-mp.loggamma(s-v+1)+v*mp.log(p)+(s-v)*mp.log1p(-p) for v in x)
 s=mp.mpf(1000023);maximum=ll(s,mean/s);public=ll(s,mp.mpf(.9999816004231903))
 results.append(dict(family='binom',index=457,digits=digits,maximum=str(maximum),public_value=str(public),gap_relative=str((maximum-public)/max(1,abs(maximum))),adjacent_profile_values=[str(ll(j,mean/j)) for j in (s-1,s+1)]))
 # The nhyper grid results are uncertified budget fallbacks. Test a feasible
 # neighbor independently; correcting their value does not certify the search.
 for index,stopping,population in ((5175,1,6000029),(5230,3,6000027)):
  x=list(map(mp.mpf,cases[index]['data']));m=mp.mpf(20);r=mp.mpf(stopping);N=mp.mpf(population)
  def choose(a,b):return mp.loggamma(a+1)-mp.loggamma(b+1)-mp.loggamma(a-b+1)
  def ll(N):return mp.fsum(choose(v-1,r-1)+choose(N-v,m-r)-choose(N,m) for v in x)
  value=ll(N);neighbor=ll(N+1)
  # C(N+1-x,m-r)/C(N-x,m-r) divided by C(N+1,m)/C(N,m).
  # Every factor is rational at these integer inputs; the sign is exact.
  ratio=Fraction(1)
  for draw in cases[index]['data']:
   assert draw==int(draw)
   ratio*=Fraction(population+1-int(draw),population+1-int(draw)-20+stopping)*Fraction(population+1-20,population+1)
  assert ratio>1
  results.append(dict(family='nhyper',index=index,digits=digits,public_value=str(value),neighbor_value=str(neighbor),neighbor_improvement=str(neighbor-value),population=population,neighbor_population=population+1,stopping=stopping,exact_neighbor_ratio=dict(numerator=str(ratio.numerator),denominator=str(ratio.denominator),strictly_greater_than_one=True)))
path=Path(__file__).with_name('maximum-values.json');path.write_text(json.dumps(results,indent=2)+'\n')
for r in results:print(r['family'],r['digits'],r.get('gap_relative',r.get('neighbor_improvement')))
