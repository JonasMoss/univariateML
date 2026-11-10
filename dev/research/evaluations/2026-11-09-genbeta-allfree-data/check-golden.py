"""Independent high-precision stationary oracle; globality is a separate claim."""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=70
root=Path.cwd();d=root/'dev/research/evaluations/2026-11-09-genbeta-allfree-data'
g=json.loads((root/'univariateML-cpp/include/cppunivariate/distributions/genbeta/golden.json').read_text())
y=[-mp.log(mp.mpf(v)) for v in g['data']];n=len(y);mean=sum(y)/n
start=[mp.log(mp.mpf(g['estimates'][k])) for k in ('shape1','shape2','shape3')]
def scores(lp,lq,la):
 p,q,a=mp.exp(lp),mp.exp(lq),mp.exp(la)
 r=-sum(mp.log(-mp.expm1(-a*v)) for v in y)/n
 D=sum(a*v/mp.expm1(a*v) for v in y)/n
 return mp.digamma(p+q)-mp.digamma(p)-a*mean,mp.digamma(p+q)-mp.digamma(q)-r,1-p*a*mean+(q-1)*D
z=mp.findroot(scores,start,tol=mp.mpf('1e-58'),maxsteps=100)
p,q,a=map(mp.exp,z)
ll=sum(mp.log(a)+v-p*a*v+(q-1)*mp.log(-mp.expm1(-a*v))-mp.log(mp.beta(p,q)) for v in y)
c=mp.log(mean)-sum(mp.log(v) for v in y)/n
kl,kh=1/(2*c),1/c
for _ in range(300):
 k=(kl+kh)/2
 if mp.log(k)-mp.digamma(k)>c:kl=k
 else:kh=k
k=(kl+kh)/2
gamma=n*(k*mp.log(k)-k-mp.loggamma(k)-(k-1)*c-mp.log(mean))+sum(y)
power=n*(-mp.log(mean-min(y))-1)+sum(y)
errors={name:float(abs(value/mp.mpf(g['estimates'][name])-1)) for name,value in zip(('shape1','shape2','shape3'),(p,q,a))}
assert max(errors.values())<1e-5
assert abs(ll-mp.mpf(g['logLik']))<mp.mpf('1e-8')
result={'precision':mp.mp.dps,'input':'exact binary64 observations, converted directly from Python float to mp.mpf','kind':'stationary coefficient and likelihood oracle; not a globality proof','p':str(p),'q':str(q),'a':str(a),'loglik':str(ll),'gamma_face_loglik':str(gamma),'moving_power_face_loglik':str(power),'relative_golden_errors':errors}
(d/'golden-oracle.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
