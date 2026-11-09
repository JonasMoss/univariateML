"""70-digit pointwise curvature cross-check, not shape-interval coverage."""
import mpmath as mp,json,pathlib
mp.mp.dps=70
root=pathlib.Path.cwd();rows=[]
for family in ['gev','frechet']:
 x=[mp.mpf(v) for v in json.loads((root/f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json').read_text())['data']]
 x.sort();m=x[0];d=x[-1]-m;x=[(v-m)/d for v in x];n=len(x)
 for xi in [mp.mpf('-.5'),mp.mpf('-.1')]:
  def pieces(u,k):
   s=mp.exp(u);pivot=x[-1] if k<0 else x[0]
   w=[(v-pivot)/s for v in x]
   L=[mp.log1p(k*z)/k for z in w]
   e=[mp.exp(-v) for v in L];A=[z/(1+k*z) for z in w]
   return L,e,A
  def score(u):
   L,e,A=pieces(u,xi)
   return (1+xi)*mp.fsum(A)/n-mp.fsum(a*b for a,b in zip(e,A))/mp.fsum(e)-1
  lo,hi=mp.mpf(-20),mp.mpf(20)
  assert score(lo)>0 and score(hi)<0
  for _ in range(40):
   mid=(lo+hi)/2
   if score(mid)>0:lo=mid
   else:hi=mid
  u=mp.findroot(score,(lo,hi),solver='secant')
  def objective(u,k):
   L,e,A=pieces(u,k)
   return -n*u+n*mp.log(n/mp.fsum(e))-n-(1+k)*mp.fsum(L)
  duu=mp.diff(objective,(u,xi),(2,0));duk=mp.diff(objective,(u,xi),(1,1))
  curvature=mp.diff(objective,(u,xi),(0,2))-duk**2/duu
  rows.append(dict(family=family,xi=str(xi),score_residual=str(score(u)),profile_curvature=str(curvature)))
assert mp.mpf(rows[0]['profile_curvature'])>0
assert mp.mpf(rows[1]['profile_curvature'])<0
print(json.dumps(dict(precision=mp.mp.dps,scope='pointwise only; does not exclude missed roots',rows=rows),indent=2))
