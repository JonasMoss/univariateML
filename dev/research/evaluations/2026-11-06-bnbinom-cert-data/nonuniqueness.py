"""Exclude every diagonal point for a concrete fixed-alpha sample.

Exact-arithmetic bounds, conditional on 60-digit comparisons with 1e-20
absolute log-likelihood margin; no outward rounding is claimed.
For l <= s <= h, p_x(s,s) <= p_0(l,l)*R_x(h), where
R_x(s)=(s)_x^2/((2+2*s)_x*x!). R_x is increasing, p_0 decreasing.
Outside [lo,hi], use the small-shape mass bound and stochastic ordering.
Together with the dossier's compact-superlevel proof this implies that
some attained maximizer is off diagonal, and symmetry gives two distinct
labeled global maximizers. This is an existence certificate, not a fit.
"""
import json
from collections import Counter
from pathlib import Path
import mpmath as mp
mp.mp.dps=60
root=Path('univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json')
x=json.loads(root.read_text())['data']; counts=Counter(x); n=len(x)
lo,hi=mp.mpf('0.000001'),mp.mpf(256)
margin=mp.mpf('1e-20')
witness=sum(c*(mp.log(60)-mp.log(k+5)-mp.log(k+6)-mp.log(k+7)) for k,c in counts.items())
target=witness-mp.mpf('.1')
def logzero(s):return 2*mp.loggamma(s+2)-mp.loggamma(2*s+2)
def logratio(s,k):return 2*(mp.loggamma(s+k)-mp.loggamma(s))-(mp.loggamma(2+2*s+k)-mp.loggamma(2+2*s))-mp.loggamma(k+1)
def upper(l,h):return n*logzero(l)+sum(c*logratio(h,k) for k,c in counts.items() if k)
left=sum(c*(mp.loggamma(lo+k)-mp.loggamma(lo)-mp.loggamma(k+1)) for k,c in counts.items() if k)
lp=logzero(hi); terms=[lp]
for k in range(1,max(x)+1):
    lp+=2*mp.log(hi+k-1)-mp.log(2+2*hi+k-1)-mp.log(k)
    terms.append(lp)
right=n*mp.log(sum(mp.exp(v) for v in terms))
assert left+margin<target and right+margin<target
queue=[(mp.log(lo),mp.log(hi))]; evaluated=0; accepted=0; max_upper=max(left,right)
while queue:
    l,h=queue.pop(); bound=upper(mp.exp(l),mp.exp(h)); evaluated+=1
    if bound+margin<target:
        accepted+=1;max_upper=max(max_upper,bound);continue
    # Work guard is only for this independent research reproducer, not a fit.
    if evaluated>1000000:raise RuntimeError('certificate work guard')
    m=(l+h)/2;queue.extend([(l,m),(m,h)])
print(json.dumps(dict(alpha=2,witness=[1,2,5],witness_logLik=str(witness),
    diagonal_logLik_upper=str(max_upper+margin),strict_gap_at_least=str(witness-max_upper-margin),
    left_upper=str(left),right_upper=str(right),range=[str(lo),str(hi)],
    segments=evaluated,accepted=accepted,precision=mp.mp.dps,margin=str(margin),
    certificate='exact arithmetic conditional on computed comparisons; practical margin, not outward rounding'),indent=2))
