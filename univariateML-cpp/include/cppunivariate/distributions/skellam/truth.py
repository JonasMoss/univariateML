"""Positive Poisson-convolution series with two geometric majorants.

For k>=0, mass=e^(-a-b) a^k/k! sum_j (ab)^j/(j!(k+1)_j).
Its inner ratio decreases, so the next term/(1-ratio) bounds the remainder.
Termwise comparison gives mass(k+1)/mass(k)<=a/(k+1). Thus once k>a,
the entire count tail has a geometric majorant too. Reflect for negative
counts. At enormous k both bounds are retained on the stored log scale.
"""
from functools import lru_cache
import mpmath as mp
from quality.truth_reference import Reference, pair


boundary_parameter_sets = [{'mu1': 0., 'mu2': 0.}]

def build(p, logd):
    a,b=p['mu1'],p['mu2']
    @lru_cache(maxsize=20000)
    def mass(k, a=a, b=b):
        if k<0:return mass(-k,b,a)
        if a==0:return -b if k==0 else -mp.inf
        term=mp.mpf(1);total=term;j=0
        while True:
            ratio=a*b/((j+1)*(k+j+1))
            if ratio<1 and term*ratio/(1-ratio)<total*mp.mpf('1e-48'):break
            j+=1;term*=ratio;total+=term
        return -a-b+k*mp.log(a)-mp.loggamma(k+1)+mp.log(total)
    @lru_cache(maxsize=20000)
    def upper(n,a,b):
        first=mass(n,a,b)
        total=mp.mpf(1);term=mp.mpf(1);k=n
        while True:
            ratio=a/(k+1)
            if ratio<1 and term*ratio/(1-ratio)<total*mp.mpf('1e-46'):break
            k+=1;term=mp.exp(mass(k,a,b)-first);total+=term
        return first+mp.log(total)
    @lru_cache(maxsize=20000)
    def tails(k):
        if k==-mp.inf:return -mp.inf,mp.mpf(0)
        if k<0:return pair(upper(-k,b,a))
        return pair(upper(k+1,a,b),False)
    for k in (-5,0,5):
        if a>0 and b>0 and abs(mass(mp.mpf(k))-logd(mp.mpf(k)))>mp.mpf('1e-45'):
            raise ArithmeticError('README density mismatch')
    ref=Reference(tails,0 if b==0 else -mp.inf,0 if a==0 else mp.inf,True,log_density=mass)
    # Independent positive body sum, with both geometric remainders omitted.
    width=int(max(a,b)+200)
    ref.normalization=mp.fsum(mp.exp(mass(k)) for k in range(-width,width+1))
    if max(tails(-width-1)[0],tails(width)[1])>mp.log(mp.mpf('1e-40')):
        raise ArithmeticError('normalization tail bound')
    return ref

# Independent rows retained outside active checks; exact findings in the TASK-106 report.
truth_exclusions = {'values': [([1.0, 0.0], [174.0], 'TASK-106 dpqr boundary mismatch; see boundary report'),
            ([1.0, 0.0], [175.0], 'TASK-106 dpqr boundary mismatch; see boundary report'),
            ([1.0, 0.0], [176.0], 'TASK-106 dpqr boundary mismatch; see boundary report')]}
