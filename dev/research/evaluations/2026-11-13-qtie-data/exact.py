"""Exact rational jump fixtures; no floating point in expected quantiles."""
from fractions import Fraction as F
from math import comb
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=100
rows=[]
def add(id,theta,masses):
 cumulative=F(0)
 for k,mass in enumerate(masses):
  cumulative+=mass
  if 0<cumulative<1 and mass:
   rows.append(dict(family=id,parameters=theta,k=k,numerator=cumulative.numerator,denominator=cumulative.denominator))
for m,N,n in [(5,10,5),(3,6,3),(7,14,7),(9,18,9)]:
 add('hyper',[m,N,n],[F(comb(m,x)*comb(N-m,n-x),comb(N,n)) for x in range(n+1)])
for n in [1,2,8,16]:
 for p in [F(1,4),F(1,2),F(3,4)]: add('binom',[n,float(p)],[comb(n,k)*p**k*(1-p)**(n-k) for k in range(n+1)])
for n in [1,2,4]:
 for p in [F(1,4),F(1,2),F(3,4)]: add('nbinom',[n,float(p)],[comb(k+n-1,k)*p**n*(1-p)**k for k in range(25)])
for p in [F(1,4),F(1,2),F(3,4)]: add('geom',[float(p)],[p*(1-p)**k for k in range(25)])
for hi in [3,7,15]: add('dunif',[0,hi],[F(1,hi+1)]*(hi+1))
Path(__file__).with_name('exact.json').write_text(json.dumps(rows,indent=2)+'\n')
# Append literal double probabilities, with exact integer-derived k.
root=Path('univariateML-cpp/include/cppunivariate/distributions')
for id in ['hyper','binom','nbinom','geom','dunif']:
 p=root/id/'test.cpp';s=p.read_text();marker=f'TEST_CASE("{id}: rational jump ties")'
 if marker in s: continue
 lines=[f'\n{marker} {{','  // Expected indices computed with Fraction in 2026-11-13-qtie-data/exact.py.']
 for row in rows:
  if row['family']!=id: continue
  # Fixtures deliberately stay away from sub-band jumps.
  v=F(row['numerator'],row['denominator']);k=row['k'];theta=','.join(repr(float(t)) for t in row['parameters'])
  if float(v)>1-1e-10: continue
  for lower in [True,False]:
   rational=v if lower else 1-v
   prob=float(rational);lowerstr=str(lower).lower()
   logprob=float(mp.log(mp.mpf(rational.numerator)/rational.denominator))
   lines.append(f'  CHECK(cppunivariate::distributions::q{id}({prob!r},{theta},{lowerstr},false)=={k});')
   lines.append(f'  CHECK(cppunivariate::distributions::q{id}({logprob!r},{theta},{lowerstr},true)=={k});')
 lines.append('}')
 p.write_text(s+'\n'.join(lines)+'\n')
