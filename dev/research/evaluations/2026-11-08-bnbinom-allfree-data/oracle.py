"""Exact rational certificate examples and independent high-precision roots."""
import json
from collections import Counter
from fractions import Fraction as F
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
out=Path(__file__).parent
sample=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json').read_text())['data']
d=Counter(sample)
def ll(p):
    r,a,b=p
    return sum(n*(mp.loggamma(r+x)-mp.loggamma(r)-mp.loggamma(x+1)+mp.loggamma(a+r)+mp.loggamma(b+x)-mp.loggamma(a+r+b+x)-mp.loggamma(a)-mp.loggamma(b)+mp.loggamma(a+b)) for x,n in d.items())
def score(r,a,b):
    common=sum(n*mp.digamma(a+r+b+x) for x,n in d.items())
    return (len(sample)*(mp.digamma(a+r)-mp.digamma(r))+sum(n*mp.digamma(r+x) for x,n in d.items())-common,
            len(sample)*(mp.digamma(a+r)+mp.digamma(a+b)-mp.digamma(a))-common,
            len(sample)*(mp.digamma(a+b)-mp.digamma(b))+sum(n*mp.digamma(b+x) for x,n in d.items())-common)
p=mp.findroot(score,(mp.mpf('1.07'),mp.mpf('2.14'),mp.mpf('5.36')),tol=mp.mpf('1e-65'))
root=list(p);mirror=[p[2],p[1],p[0]]
assert max(abs(v) for v in score(*p))<mp.mpf('1e-60')
assert abs(ll(root)-ll(mirror))<mp.mpf('1e-65')
# Independently optimize on the singular diagonal (observed saddle allowed).
s,a=mp.findroot(lambda s,a:(score(s,a,s)[0],score(s,a,s)[1]),(mp.mpf('2.5'),mp.mpf('2.1')),tol=mp.mpf('1e-65'))
diagonal=[s,a,s]
assert ll(root)>ll(diagonal)+mp.mpf('.1')
# Exact quotient coefficients: cumulative sums of the directional derivative.
counts=[3,10,0,1];mean=F(13,14)
proofs=[]
for label,r in [('poisson',None),('nb-size-one',F(1))]:
    q=mean if r is None else mean/(r+mean)
    c=[(k+1)*(counts[k+1] if k+1<len(counts) else 0)-(q if r is None else (k+r)*q)*nk for k,nk in enumerate(counts)]
    assert sum(c)==0
    Q=[sum(c[:j+1]) for j in range(len(c)-1)]
    discriminant=Q[1]**2-4*Q[0]*Q[2]
    assert Q[0]>0 and Q[2]>0 and discriminant<0
    assert sum(1 for u,v in zip(c,c[1:]) if u*v<0)==3
    # Verify the same Bernstein split independently in exact rationals.
    pending=[([Q[0],Q[1]/2,Q[2]],F(0),F(1))]
    leaves=[]
    while pending:
        current,lo,hi=pending.pop()
        if all(v>0 for v in current):
            leaves.append(dict(interval=[str(lo),str(hi)],bernstein=list(map(str,current))))
            continue
        assert hi-lo>F(1,1024)
        left=[current[0]];right=[current[-1]]
        while len(current)>1:
            current=[(u+v)/2 for u,v in zip(current,current[1:])]
            left.append(current[0]);right.append(current[-1])
        mid=(lo+hi)/2
        pending.extend([(left,lo,mid),(right[::-1],mid,hi)])
    proofs.append(dict(face=label,derivative_coefficients=list(map(str,c)),quotient=list(map(str,Q)),discriminant=str(discriminant),bernstein_leaves=leaves))
result=dict(dps=80,root=list(map(str,root)),mirror=list(map(str,mirror)),loglik=str(ll(root)),scores=list(map(str,score(*p))),diagonal=list(map(str,diagonal)),diagonal_loglik=str(ll(diagonal)),exact_face_proofs=proofs)
(out/'oracle.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
