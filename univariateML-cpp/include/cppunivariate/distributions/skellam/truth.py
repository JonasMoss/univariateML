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
    if max(a,b)>1000000 and 0<min(a,b)<max(a,b)/16:
        return asymmetric_reference(a,b)
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


def asymmetric_reference(a,b):
    """Bessel masses and positive Poisson convolutions with geometric bounds.

    Canonical rates A>=B. For the lower count tail, Bessel recurrence and
    I_(n+1)(z)/I_n(z)<1 give mass(k-1)/mass(k)<sqrt(B/A)+max(k,0)/A.
    The I ratio is the expectation of t in (-1,1) under the positive
    integral weight exp(z*t)*(1-t*t)^(n-1/2); integration by parts proves
    this representation and the strict bound.

    Near the mode condition on the smaller Poisson count. The lower-CDF
    convolution term ratio is <=B/(j+1)*(1+A/(k+j+1)); the upper one is
    <=B/(j+1)*min(1,A/(k+j+2)). Both decrease, giving global geometric
    majorants on omitted positive terms. No cancellation of tiny tails.
    """
    from quality.truth_reference import gamma
    A,B=max(a,b),min(a,b)
    if not 0<B<A/16:raise ArithmeticError('Skellam asymmetric route domain')
    z=2*mp.sqrt(A*B);logratio=mp.log(A/B)/2
    @lru_cache(maxsize=50000)
    def mass(k):
        return -A-B+k*logratio+mp.log(mp.besseli(abs(k),z))
    @lru_cache(maxsize=50000)
    def poisson(k):return gamma(k+1,A)[::-1]
    @lru_cache(maxsize=20000)
    def canonical(k):
        if k==-mp.inf:return -mp.inf,mp.mpf(0)
        if k==mp.inf:return mp.mpf(0),-mp.inf
        q=mp.sqrt(B/A)+max(k,0)/A
        # Termwise positive 0F1 comparison bounds I_(n+1)/I_n.
        if k<=0:q=min(q,B/(1-k))
        if q<mp.mpf('.5'):
            first=mass(k);term=mp.mpf(1);total=term
            for j in range(10000):
                if term*q/(1-q)<total*mp.mpf('1e-50'):
                    return pair(first+mp.log(total))
                term=mp.exp(mass(k-j-1)-first);total+=term
            raise ArithmeticError('Skellam lower-count geometric remainder')
        lower=k<=A-B;index=0 if lower else 1
        first=-B+poisson(k)[index]
        term=mp.mpf(1);total=term
        previous=poisson(k)[index]
        poisson_mass=-A+k*mp.log(A)-mp.loggamma(k+1)
        for j in range(10000):
            majorant=B/(j+1)*(1+A/(k+j+1) if lower else min(1,A/(k+j+2)))
            if majorant<1 and term*majorant/(1-majorant)<total*mp.mpf('1e-50'):
                return pair(first+mp.log(total),lower)
            # The adjacent Poisson cut differs by one positive PMF.
            # Avoid recomputing a concentrated gamma integral at every cut.
            poisson_mass+=mp.log(A)-mp.log(k+j+1)
            ratio=mp.exp(poisson_mass-previous)
            if lower:
                current=previous+mp.log1p(ratio)
            elif ratio<1-mp.mpf('1e-100'):
                current=previous+mp.log1p(-ratio)
            else:
                current=poisson(k+j+1)[index]
            term*=B/(j+1)*mp.exp(current-previous);total+=term;previous=current
        raise ArithmeticError('Skellam positive convolution remainder')
    def tails(k):return canonical(k) if a>=b else canonical(-k-1)[::-1]
    ref=Reference(tails,-mp.inf,mp.inf,True,
                  log_density=lambda k:mass(k if a>=b else -k))
    # Independent Poisson masses each sum to one; their product law and
    # the difference map preserve total mass (Tonelli's theorem).
    ref.normalization=mp.mpf(1)
    return ref
