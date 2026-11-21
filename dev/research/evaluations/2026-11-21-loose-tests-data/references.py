#!/usr/bin/env python3
"""Recompute independent references from the repository root.

Python float parses decimal test inputs to binary64 before mpmath sees them.
The two solves must round identically. A score root is numerical evidence.
"""
import csv
import gzip
from collections import Counter
from fractions import Fraction
import math
import json
from pathlib import Path
import re

import mpmath as mp


OUT = Path(__file__).resolve().parent
DIST = Path('univariateML-cpp/include/cppunivariate/distributions')


def vector(family, case, name='x'):
    text = (DIST / family / 'test.cpp').read_text()
    body = text.split(case, 1)[1]
    values = re.search(r'\b' + name + r'\s*=?\s*\{([^{}]+)\}', body)[1]
    return [mp.mpf(float(x)) for x in values.split(',') if x.strip()]


def fixture(family, name='golden'):
    raw = json.loads((DIST / family / f'{name}.json').read_text())
    return [mp.mpf(float(x)) for x in raw['data']], {
        k: mp.mpf(float(v)) for k, v in raw['estimates'].items()
    }


def root(score, start):
    result = mp.findroot(score, start, maxsteps=100)
    coordinates = tuple(result) if isinstance(result, mp.matrix) else (result,)
    residual = score(*coordinates)
    if not isinstance(residual, (list, tuple)):
        residual = (residual,)
    assert max(map(abs, residual)) < mp.mpf(10)**(12-mp.mp.dps)
    return coordinates


def stationary(loglik, start):
    def score(*p):
        return tuple(mp.diff(loglik, p, tuple(int(i == j) for j in range(len(p))))
                     for i in range(len(p)))
    return root(score, start)


def positive_stationary(loglik, start):
    return tuple(map(mp.exp, stationary(
        lambda *p: loglik(*map(mp.exp, p)), tuple(map(mp.log, start)))))


def beta_prime(x, a, start):
    a = mp.mpf(a)
    n = len(x)
    def score(b, ls):
        s = mp.exp(ls)
        return (n*(mp.digamma(a+b)-mp.digamma(b))-mp.fsum(mp.log1p(v/s) for v in x),
                -n*a+(a+b)*mp.fsum(v/(s+v) for v in x))
    b, ls = root(score, (start[0], mp.log(start[1])))
    return b, mp.exp(ls)


def bnbinom(x, hold, start):
    counts = Counter(x)
    def ll(size, a, b):
        return mp.fsum(n*(mp.loggamma(k+size)-mp.loggamma(size)
                          +mp.loggamma(k+b)+mp.loggamma(size+a)
                          -mp.loggamma(k+size+a+b)
                          +mp.loggamma(a+b)-mp.loggamma(a)-mp.loggamma(b))
                       for k,n in counts.items())
    def profiled(*p):
        it = iter(p)
        return ll(*(next(it) if v is None else mp.mpf(v) for v in hold))
    return positive_stationary(profiled, start)


def normal(x):
    mu = mp.fsum(x)/len(x)
    return mu, mp.sqrt(mp.fsum((v-mu)**2 for v in x)/len(x))


def laplace(x):
    mu = sorted(x)[len(x)//2]
    return mu, mp.fsum(abs(v-mu) for v in x)/len(x)


def lomax(x, initial):
    x = list(map(lambda v: mp.mpf(float(v)), x))
    def shape(rate):
        return len(x)/mp.fsum(mp.log1p(rate*v) for v in x)
    def score(lograte):
        rate = mp.exp(lograte)
        return len(x)-(shape(rate)+1)*mp.fsum(rate*v/(1+rate*v) for v in x)
    rate = mp.exp(root(score, mp.log(initial))[0])
    return rate, shape(rate)


def zip_reference(x, start):
    npositive = sum(v > 0 for v in x)
    mean_positive = sum(x)/mp.mpf(npositive)
    rate, = root(lambda rate: rate/(-mp.expm1(-rate))-mean_positive, start)
    return rate, 1-npositive/(len(x)*(-mp.expm1(-rate)))


def student(x, mu, sd, df=None, standard=False):
    def ll(m, s, nu):
        width2 = nu-2 if standard else nu
        return (len(x)*(mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)
                        -mp.log(width2)/2-mp.log(s))
                -(nu+1)/2*mp.fsum(mp.log1p((v-m)**2/(width2*s*s)) for v in x))
    if df is None:
        return positive_stationary(lambda d: ll(mu, sd, d), (4 if standard else 3,))
    return stationary(lambda m: ll(m, sd, df), (mu,))


def references():
    x = vector('betapr', 'conditional coefficients match independent')
    for i, (a,b,s) in enumerate(zip((.7,2,5),(1.7,.64,.486),(3.1,.122,.021))):
        yield f'betapr/conditional/{i}', beta_prime(x,a,(b,s))
    x = vector('betapr', 'conditional certificate compares separated')
    yield 'betapr/separated', beta_prime(x,.22690393135372058,(.22,2.55))
    x = vector('betapr', 'flat gamma-adjacent conditional maximum')
    yield 'betapr/flat', beta_prime(x,.7,(95.5,78.2))
    x = list(map(mp.mpf, (0,0,1,2,3,6,12,24)))
    for i, guess in enumerate((1.23,.79,22.08)):
        held = [.37,1.8,4.6]
        held[i] = None
        yield f'bnbinom/scalar/{i}', bnbinom(x,held,(guess,))
    yield 'bnbinom/diagonal', bnbinom(x,(None,.5,None),(1.11,1.11))
    x, _ = fixture('bnbinom')
    yield 'bnbinom/off-diagonal', bnbinom(x,(None,2,None),(1.15,4.5))
    yield 'bnbinom/legacy', bnbinom(x,(None,None,None),(1.07,2.14,5.36))

    for family in ('laplace','norm','lnorm'):
        x,_ = fixture(family,'fixture-outlier')
        if family == 'lnorm':
            x = list(map(mp.log,x))
        yield f'{family}/outlier', laplace(x) if family == 'laplace' else normal(x)

    for i,(x,start) in enumerate(zip(((1,100),(.001,4,6),(.001,1,2),(1,29),(1,1,1,12,12)),
                                     (.772,1394,1039,.237,.036))):
        yield f'lomax/{i}', lomax(x,start)
    for i,(x,start) in enumerate(zip(((0,1,2,1,1,2,0,3,0,0,0,0,0,0,0,0,0,2,0,3),(0,0,4)),
                                     (1.42,3.92))):
        yield f'zip/{i}', zip_reference(x,start)
    for family, location, scale, shape in (('lst','mu','sigma','df'), ('std','mean','sd','nu')):
        x,g = fixture(family)
        yield f'{family}/shape', student(x,g[location],g[scale],standard=family=='std')
    yield 'lst/location', student(list(map(mp.mpf,(-3,-2,0,1,5))), -.32, 1, df=3)

    x = vector('genbeta', 'coefficient accuracy on audit case 2613')
    b,c = map(mp.mpf,(.24298710443400298,4.084996007320842))
    yield 'genbeta/2613', root(lambda a: len(x)*(mp.digamma(a+b)-mp.digamma(a))
                              + c*mp.fsum(map(mp.log,x)), .0063)
    x,g = fixture('genbeta')
    logs = list(map(mp.log,x))
    def gb(a,b,p):
        return len(x)*(mp.log(p)-mp.log(mp.beta(a,b)))+mp.fsum(
            (p*a-1)*v+(b-1)*mp.log(-mp.expm1(p*v)) for v in logs)
    yield 'genbeta/held-scale', positive_stationary(gb,[g[k] for k in ('shape1','shape2','shape3')])

    x = vector('invburr', 'default multistart retains the seed-3 stationary fit')
    logs = [-mp.log(v) for v in x]
    def burr(a,b,s):
        return len(x)*(mp.log(a*b)-mp.log(s))+mp.fsum(
            (b-1)*(v-mp.log(s))-(a+1)*mp.log1p(mp.exp(b*(v-mp.log(s))))
            for v in logs)
    a,b,s = positive_stationary(burr,(1.2,2.15,3.25))
    yield 'invburr/interior', (a,b,1/s)

    for pi, start in ((.7,.95745),(.8,.50682)):
        r = 10
        pi = mp.mpf(pi)
        def score(p):
            return (99*(1-pi)*r*p**(r-1)/(pi+(1-pi)*p**r)
                    +r/p-10/(1-p))
        yield f'zinb/held-inflation/{float(pi)}', root(score,start)

    raw = json.loads(Path('univariateML-cpp/tests/modifiers/fixtures/zinb/golden.json').read_text())
    g = {k: mp.mpf(v) for k,v in raw['estimates'].items()}
    positive = Counter(k for k in raw['data'] if k>0)
    n = len(raw['data'])
    zero = n-sum(positive.values())
    def zinb(size,p,pi):
        return zero*mp.log(pi+(1-pi)*p**size)+mp.fsum(
            f*(mp.log1p(-pi)+mp.loggamma(k+size)-mp.loggamma(size)
               +size*mp.log(p)+k*mp.log1p(-p)) for k,f in positive.items())
    def free_pi(size,p):
        mass = p**size
        return (mp.mpf(zero)/n-mass)/(1-mass)
    for mask in range(4):
        size, p, pi = (g[key] for key in ('size','prob','pi'))
        if not mask&1:
            def obj(q):
                return zinb(size,q,pi if mask&2 else free_pi(size,q))
            p, = stationary(obj,(p,))
        if not mask&2:
            pi = free_pi(size,p)
        yield f'zinb/held-size/{mask}', (p,pi)
    for j, pi in enumerate((None,g['pi'])):
        p = g['prob']
        size, = stationary(lambda r: zinb(r,p,free_pi(r,p) if pi is None else pi),(g['size'],))
        yield f'zinb/held-prob/{j}', (size,)

    # CMP recovery uses integer multiplicities, not a random draw.
    counts = []
    p = math.exp(-3.)
    for k in range(20):
        counts.append(round(100000*p))
        p *= 3./(k+1)
    n = sum(counts)
    mean = mp.fsum(k*c for k,c in enumerate(counts))/n
    h = [mp.loggamma(k+1) for k in range(170)]
    mean_h = mp.fsum(c*v for c,v in zip(counts,h))/n
    def cmp(eta, nu):
        w = [mp.exp(k*eta-nu*v) for k,v in enumerate(h)]
        z = mp.fsum(w)
        assert w[-1] < mp.mpf(10)**(-mp.mp.dps-10)
        return (mp.fsum(k*v for k,v in enumerate(w))/z-mean,
                mp.fsum(v*q for v,q in zip(w,h))/z-mean_h)
    eta,nu = root(cmp,(mp.log(3),1))
    yield 'cmp/recovery', (nu,mp.exp(eta))

    def ged_ll(x, mu, sd, nu):
        width = sd*mp.exp((mp.loggamma(1/nu)-mp.loggamma(3/nu))/2)
        return len(x)*(mp.log(nu)-mp.log(width)-mp.loggamma(1/nu))-mp.fsum(
            (abs(v-mu)/width)**nu for v in x)

    x,g = fixture('ged')
    for mask in range(4):
        def ll(*v):
            it = iter(v)
            mu = g['mean'] if mask&1 else next(it)
            sd = g['sd'] if mask&2 else mp.exp(next(it))
            return ged_ll(x,mu,sd,mp.exp(next(it)))
        start = ([] if mask&1 else [g['mean']])+([] if mask&2 else [mp.log(g['sd'])])+[mp.log(g['nu'])]
        fit = iter(stationary(ll,tuple(start)))
        yield f'ged/golden/{mask}', (g['mean'] if mask&1 else next(fit),
                                    g['sd'] if mask&2 else mp.exp(next(fit)),mp.exp(next(fit)))
    tied = [mp.mpf(0)]*8+[mp.mpf(float(.32998660809120006))]*46+[mp.mpf(float(1.4239025460520294))]*24
    yield 'ged/two-shapes', tuple(stationary(lambda v: ged_ll(tied,0,1,v),(p,))[0]
                                  for p in (1.108,25.4))

    x = vector('ht','outer certificate handles multiple modes')
    def t_ll(x, mu, sd, nu, variance=False):
        width = (nu-2 if variance else nu)*sd**2
        return (len(x)*(mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(width)/2)
                -(nu+1)/2*mp.fsum(mp.log1p((v-mu)**2/width) for v in x))
    yield 'ht/outer', positive_stationary(lambda nu,s: t_ll(x,0,s,nu),(.415,1.6))
    x = vector('std','held-scale and represented-scale precision guards')
    yield 'std/held-location', student(x,.2,mp.mpf(1e100),df=3,standard=True)
    yield 'lst/held-location', student(x,.2,mp.mpf(1e100),df=3)
    r = Path('bindings/r/distributions/std/test.R').read_text().split('certificate resolves the shallow normal-0 maximum',1)[1]
    x = [mp.mpf(float(v)) for v in re.search(r'x <- c\(([^)]+)\)',r)[1].split(',')]
    mu,ls,ln = stationary(lambda m,s,n: t_ll(x,m,mp.exp(s),mp.exp(n),True),
                          (-.024,mp.log(1),mp.log(218)))
    yield 'std/R-shallow', (mp.exp(ln),mu)

    x,g = fixture('sged')
    xi = mp.mpf(1.4)
    def sged(c,logwidth,lognu):
        width,nu = mp.exp(logwidth),mp.exp(lognu)
        z = [(v-c)/width for v in x]
        return len(x)*(mp.log(nu)-mp.loggamma(1/nu)-logwidth-mp.log(xi+1/xi))-mp.fsum(
            (abs(v)/(xi if v>=0 else 1/xi))**nu for v in z)
    c,w,l = stationary(sged,(g['mean']-.3,mp.log(1.5),mp.log(3.13)))
    yield 'sged/held-skew', (mp.exp(l),)

    x = vector('huber','near-constant sample is an estimate',name='near')
    gap = x[-1]-x[0]
    e = mp.mpf(1.345)
    # The two lower observations are in the quadratic region and the upper
    # observation is in the linear tail. The location and scale scores give gap/sigma = 3/e.
    yield 'huber/near-tie', (gap*e/3,)

    x = vector('expweibull','relaxed coefficient tolerance is honored')
    logs = list(map(mp.log,x))
    def ew(s,k,a):
        y = [mp.exp(k*(v-mp.log(s))) for v in logs]
        return len(x)*mp.log(k*a/s)+mp.fsum(
            (k-1)*(v-mp.log(s))-z+(a-1)*mp.log(-mp.expm1(-z))
            for v,z in zip(logs,y))
    yield 'expweibull/relaxed', positive_stationary(ew,(mp.exp(-.0001425075),
             mp.exp(10.066943359),mp.exp(29.64628926)))

    x, g = fixture('pig')
    for label,counts,start in [('small',[0,0,0,0,10],.0368),
                               ('golden',list(map(int,x)),g['shape'])]:
        mu = mp.fsum(counts)/len(counts)
        hist = {k: counts.count(k) for k in set(counts)}
        def pig(lam):
            # Integrate the Poisson mass against the inverse Gaussian mixing law.
            a = 2+lam/mu**2
            z = mp.sqrt(a*lam)
            return len(counts)*(mp.log(lam/(2*mp.pi))/2+lam/mu+mp.log(2))+mp.fsum(
                n*((k-mp.mpf('.5'))*mp.log(lam/a)/2+mp.log(mp.besselk(k-mp.mpf('.5'),z)))
                for k,n in hist.items())
        yield f'pig/{label}', positive_stationary(pig,(start,))

    r = Path('bindings/r/distributions/foldnorm/test.R').read_text().split('compares two distinct interior profile maxima',1)[1]
    lam = mp.mpf(1e20)
    def pig_fixed(mu):
        t = lam/mu
        z = mp.sqrt(t*t+2*lam)
        def polynomial(k):
            order = max(k-1,0)
            return mp.fsum(mp.factorial(order+j)/(
                mp.factorial(j)*mp.factorial(order-j)*(2*z)**j) for j in range(order+1))
        return -8*lam/(t+z)+6*mp.log(lam/z)+mp.fsum(mp.log(polynomial(k)) for k in range(4))
    yield 'pig/held-shape', stationary(pig_fixed,(1.5,))

    x = [mp.mpf(float(v)) for v in re.search(r'x <- c\(([^)]+)\)',r)[1].split(',')]
    m2 = mp.fsum(v*v for v in x)/len(x)
    def coeff(a):
        v = 2*m2/(1+mp.sqrt(1+4*m2*a*a))
        return a*v, mp.sqrt(v)
    candidates = []
    for i,a0 in enumerate((1.32,2.02,3.84)):
        a, = root(lambda a: mp.fsum(v*mp.tanh(a*v) for v in x)/len(x)-coeff(a)[0],a0)
        mu,sigma = coeff(a)
        yield f'foldnorm/profile/{i}', (mu,sigma)
        ll = mp.fsum(mp.log(mp.exp(-(v-mu)**2/(2*sigma*sigma))
                             +mp.exp(-(v+mu)**2/(2*sigma*sigma))) for v in x)-len(x)*mp.log(sigma)
        candidates.append((ll,mu,sigma))
    _,mu,sigma = max(candidates)
    yield 'foldnorm/best', (mu,sigma)

    # With two points and power > 1 the strictly convex weighted
    # location loss has this unique stationary point for the join.
    nu,xi = mp.mpf(1.01),mp.mpf(100)
    gamma_ratio = mp.gamma(2/nu)/mp.sqrt(mp.gamma(1/nu)*mp.gamma(3/nu))
    shift = gamma_ratio*(xi-1/xi)
    moment_scale = mp.sqrt((1-gamma_ratio**2)*(xi**2+xi**-2)+2*gamma_ratio**2-1)
    mode = mp.mpf(1e100)/(1+xi**(2*nu/(nu-1)))
    yield 'sged/loose-location', (mode+shift/moment_scale,)


def huber(x, start):
    n = len(x)
    def score(mu,ls,le):
        s,e = mp.exp(ls),mp.exp(le)
        normalizer = mp.sqrt(2*mp.pi)*mp.erf(e/mp.sqrt(2))+2*mp.exp(-e*e/2)/e
        z = [(v-mu)/s for v in x]
        psi = [min(e,max(-e,v)) for v in z]
        return (mp.fsum(psi),mp.fsum(v*w for v,w in zip(z,psi))-n,
                2*n*mp.exp(-e*e/2)/(e*e*normalizer)
                -mp.fsum(abs(v)-e for v in z if abs(v)>e))
    mu,ls,le = root(score,(start[0],mp.log(start[1]),mp.log(start[2])))
    return (mp.exp(le),mp.exp(ls))


def tnorm(x, a, b, start):
    n = len(x)
    mean = mp.fsum(x)/n
    variance = mp.fsum((v-mean)**2 for v in x)/n
    def score(mu,ls):
        s = mp.exp(ls)
        lo,hi = (a-mu)/s,(b-mu)/s
        mass = (mp.erfc(lo/mp.sqrt(2))-mp.erfc(hi/mp.sqrt(2)))/2
        pa,pb = (mp.exp(-v*v/2)/mp.sqrt(2*mp.pi) for v in (lo,hi))
        edge_a = 0 if not mp.isfinite(lo) else lo*pa
        edge_b = 0 if not mp.isfinite(hi) else hi*pb
        return ((mean-mu)/s-(pa-pb)/mass,
                (variance+(mean-mu)**2)/s**2-1-(edge_a-edge_b)/mass)
    mu,ls = root(score,(start[0],mp.log(start[1])))
    return mu,mp.exp(ls)


def dweibull(x, start):
    counts = Counter(x)
    bins = [(n,mp.log(k),mp.log1p(mp.mpf(1)/k)) for k,n in counts.items() if k]
    def score(lograte,logshape):
        rate, shape = mp.exp(lograte),mp.exp(logshape)
        rate_terms = [counts.get(0,0)*rate/mp.expm1(rate)]
        shape_terms = []
        for n, logk, logratio in bins:
            h = rate*mp.exp(shape*logk)
            gap = h*mp.expm1(shape*logratio)
            factor = 1/mp.expm1(gap)
            rate_terms.append(n*(-h+factor*gap))
            shape_terms.append(n*(-h*logk+factor*(gap*logk+(h+gap)*logratio)))
        return mp.fsum(rate_terms),mp.fsum(shape_terms)
    lograte,logshape = root(score,(mp.log(-mp.log(start[0])),mp.log(start[1])))
    return mp.exp(-mp.exp(lograte)),mp.exp(logshape)


def sample_references():
    import importlib.util
    old = OUT.parent/'2026-11-21-test-refs-data/fit-references.py'
    spec = importlib.util.spec_from_file_location('previous_references', old)
    previous = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(previous)
    r_samples = {}
    for mode in ('native','double'):
        path = OUT/f'r-{mode}-inputs.txt.gz'
        if not path.exists():
            continue
        with gzip.open(path,'rt') as f:
            for line in f:
                _,site,hexes = line.strip().split(';')
                key = site, hexes
                if key in r_samples:
                    yield f'R/{mode}/{site}', r_samples[key]
                    continue
                x = list(map(float.fromhex,hexes.split(',')))
                family = site.split('/')[0]
                if family == 'dweibull':
                    start = (.8,1.5) if site.endswith(':97') else (.9,.3)
                    values = dweibull(list(map(int,x)),start)
                elif family == 'huber':
                    values = huber(list(map(mp.mpf,x)),(2,3,.8))
                elif family in ('laplace','norm','lnorm'):
                    data = list(map(mp.mpf, x))
                    if family == 'lnorm':
                        data = list(map(mp.log,data))
                    values = laplace(data) if family == 'laplace' else normal(data)
                else:
                    start = (-2.4,.3) if site.endswith(':99') else (0,1 if family in ('cauchy','llogis') else 5)
                    values = tuple(map(mp.mpf,previous.solve(family,x,start,mp.mp.dps)))
                    if family == 'llogis':
                        # The earlier C++ outlier fixture used reciprocal
                        # scale. The public R coefficient is scale.
                        values = (values[0],1/values[1])
                r_samples[key] = values
                yield f'R/{mode}/{site}', values

    computed = {}
    for mode in ('native','double'):
        path = OUT/f'quantiles-{mode}.txt'
        if not path.exists():
            continue
        for line in path.read_text().splitlines():
            label,*hexes = line.split(',')
            if line in computed:
                yield f'{mode}/{label}', computed[line]
                continue
            x = [mp.mpf(float.fromhex(v)) for v in hexes]
            if label=='std':
                def score(mu,ls,nu):
                    width = mp.exp(2*ls)*nu
                    residual = [v-mu for v in x]
                    den = [width+v*v for v in residual]
                    weighted = mp.fsum(v*v/w for v,w in zip(residual,den))
                    logs = mp.fsum(mp.log1p(v*v/width) for v in residual)
                    n = len(x)
                    gamma_score = (mp.digamma((nu+1)/2)-mp.digamma(nu/2))/2
                    return (mp.fsum(v/w for v,w in zip(residual,den)), n-(nu+1)*weighted,
                            n*(gamma_score-1/(2*nu)) - (logs-(nu+1)*weighted/nu)/2)
                mu,ls,nu = root(score,(0,mp.log(1.0013),539.46))
                computed[line] = (nu,mp.exp(ls),mu)
            elif label=='huber':
                computed[line] = huber(x,(2,3,.8))
            else:
                a,b,*data = x
                starts=((.3,.25),(0,1),(2,1),(0,1),(0,1),(5,3),(-3,2))
                index = int(label.split('/')[1])
                computed[line] = tnorm(data,a,b,starts[index])
            yield f'{mode}/{label}', computed[line]


def integer_references():
    # Adjacent-cell comparisons check the rounded grid winners. The existing
    # tests exercise the fitters' discrete searches and tail certificates.
    for label, counts, sizes, winner in (
        ('small',(2,6,6),range(6,81),13),
        ('large',(1000000,1000001,1000003,1000008,1000011),range(1000011,1000090),1000023),
    ):
        mean = mp.fsum(counts)/len(counts)
        def profile(size):
            p = mean/size
            return mp.fsum(mp.loggamma(size+1)-mp.loggamma(size-x+1)
                           +x*mp.log(p)+(size-x)*mp.log1p(-p) for x in counts)
        best = max(sizes, key=profile)
        assert best==winner
        yield f'binom/{label}-grid', (mp.mpf(best),profile(best)-max(profile(best-1),profile(best+1)))

    x = (1000000,1000001,1000003,1000008,1000011)
    size,r = 20,3
    def population_ratio(n):
        return math.prod(Fraction(n+1-k,n+1-k-(size-r)) for k in x)*(
            Fraction(n+1-size,n+1)**len(x))
    best = 6666697
    assert population_ratio(best-1)>1>population_ratio(best)
    yield 'nhyper/population-neighbors', (mp.mpf(best),)

    def conditional_binomial_profile(x, counts, n, lower, upper, dispersion):
        support = tuple(range(lower,min(n,upper)+1))
        intercept = tuple(dispersion*mp.log(math.comb(n,k)) for k in support)
        total = sum(counts)
        average = mp.fsum(k*c for k,c in zip(x,counts))/total
        origin = mp.log(average/(n-average))
        def weights(eta):
            return [mp.exp(a+eta*k) for a,k in zip(intercept,support)]
        def score(eta):
            w = weights(eta)
            return mp.fsum((k-average)*v for k,v in zip(support,w))/mp.fsum(w)
        left, right = origin-4,origin+4
        assert score(left)<0<score(right)
        eta = mp.findroot(score,(left,right),solver='bisect',maxsteps=400,verify=False)
        assert abs(score(eta)) < mp.mpf(10)**(12-mp.mp.dps)
        objective = mp.fsum(c*(intercept[k-lower]+k*eta) for k,c in zip(x,counts))
        return objective-total*mp.log(mp.fsum(weights(eta)))

    for label, x, counts, lower, cap, answer in (
        ('positive',(1,2,3,4,5),(1,2,3,2,2),1,64,6),
        ('zero-second-order',(0,1,2,3,4),(242187,60672,121344,1488980,406274),0,4,7),
    ):
        values = {n:conditional_binomial_profile(x,counts,n,lower,cap,1)
                  for n in range(max(x),65)}
        best = max(values,key=values.get)
        assert best==answer
        yield f'tbinom/{label}-grid', (mp.mpf(best),values[best]-max(values[best-1],values[best+1]))

    x,g = fixture('cmb')
    histogram = sorted(Counter(map(int,x)).items())
    support, counts = zip(*histogram)
    values = {n:conditional_binomial_profile(support,counts,n,0,n,g['nu'])
              for n in range(max(support),41)}
    best = max(values,key=values.get)
    assert best==10
    yield 'cmb/fixed-shape-grid', (mp.mpf(best),)


def main():
    matrices = []
    for digits in (70,100):
        with mp.workdps(digits):
            matrices.append([(case, [mp.nstr(v,digits) for v in values])
                             for case,values in (*references(), *sample_references(), *integer_references())])
    with (OUT/'references.csv').open('w',newline='') as f:
        writer = csv.writer(f,lineterminator='\n')
        writer.writerow(['case','index','root_100_digits','binary64','hex'])
        for (case, earlier), (again, values) in zip(*matrices,strict=True):
            assert case == again and list(map(float,earlier)) == list(map(float,values)), case
            for i,v in enumerate(values):
                writer.writerow([case,i,v,repr(float(v)),float(v).hex()])


if __name__ == '__main__':
    main()
