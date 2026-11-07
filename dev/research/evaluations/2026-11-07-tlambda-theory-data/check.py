"""Independent research checks, not package regression tests or proofs.

Run with an existing Python environment containing mpmath/numpy/scipy.
No package installation, repo imports, seeds from the package, or fitter calls.
"""
from fractions import Fraction as F
from pathlib import Path
import hashlib
import json
import platform
import subprocess
import sys
import mpmath as mp
import numpy as np
import scipy
from scipy.optimize import differential_evolution

ROOT = Path(__file__).resolve().parents[4]
OUT = Path(__file__).resolve().parent
mp.mp.dps = 120
checks = {}
print('Research checks started (120 decimal digits)',flush=True)


def q(u, l):
    return mp.log(u / (1-u)) if not l else (u**l-(1-u)**l)/l


def d(u, l):
    return u**(l-1)+(1-u)**(l-1)


def curvature(u, b):
    v = 1-u
    D = u**(-b)+v**(-b)
    D1 = b*(v**(-b-1)-u**(-b-1))
    D2 = b*(b+1)*(u**(-b-2)+v**(-b-2))
    direct = D*D2-2*D1**2
    expanded = b*((1-b)*(u**(-2*b-2)+v**(-2*b-2))
                  +(b+1)*(u**(-b)*v**(-b-2)+v**(-b)*u**(-b-2))
                  +4*b*u**(-b-1)*v**(-b-1))
    return direct, expanded


u = F(1, 10)
direct, expanded = curvature(u, 2)
assert direct == expanded and direct < 0
checks['exact_negative_shape_curvature'] = str(direct)
for u in (F(1, 10), F(1, 3), F(1, 2)):
    # Integer b=1 is an exact rational identity; noninteger b checked below.
    direct, expanded = curvature(u, 1)
    assert direct == expanded and direct > 0
checks['exact_logistic_curvature'] = 'positive at three rational points'

# Sharper lambda=-1 envelope: D=Q^2+2/[u(1-u)] >= Q^2+8.
for p in (F(1,10),F(1,3),F(1,2)):
    Q = (2*p-1)/(p*(1-p))
    D = p**-2+(1-p)**-2
    assert D-Q*Q == 2/(p*(1-p)) and D-Q*Q >= 8
checks['exact_minus1_cauchy_envelope'] = 'three rational identities and signs'

points = [mp.mpf(10)**(-j) for j in (1, 2, 4, 10, 25)] + [mp.mpf('.2'), mp.mpf('.49'), mp.mpf('.5')]
grid_count = 0
max_env = mp.mpf(0)
for ls in ('-1', '-.99', '-.5', '-1e-8', '0', '1e-8', '.1', '.5', '.99', '1'):
    l = mp.mpf(ls)
    for p in points:
        Q, D = q(p, l), d(p, l)
        assert 1/D <= mp.mpf('.5')*(1+mp.mpf('1e-60'))
        env = Q**2/D
        assert env <= 1+mp.mpf('1e-60')
        max_env = max(max_env, env)
        if l < 1:
            direct, expanded = curvature(p, 1-l)
            assert mp.almosteq(direct, expanded, rel_eps=mp.mpf('1e-55'))
            if l >= 0:
                assert direct > 0
        if p < mp.mpf('.5'):
            assert mp.diff(lambda sh: -q(p, sh), l) < 0
        grid_count += 1
checks['principal_envelope_and_shape_monotonicity'] = dict(points=grid_count, max_z2g=str(max_env))

count = 0
for As in ('.25', '1', '3', '20'):
    A = mp.mpf(As)
    r = 1+1/A
    C = max(mp.mpf(1), A**(-r))
    for l in (-A, -A/2, mp.mpf(0), mp.mpf('.5'), mp.mpf(1)):
        for p in points:
            assert abs(q(p, l))**r/d(p, l) <= C*(1+mp.mpf('1e-55'))
            count += 1
checks['extended_shape_envelope'] = count

count = 0
for ls in ('-20', '-3', '-1', '-.1', '.1', '.5', '.99', '1.5', '2', '5'):
    l = mp.mpf(ls)
    for p in points[:-1]:
        z = -q(p, l)
        if z == 0:
            continue
        if l < 0:
            a = -l
            low = (a*z+2**a)**(-1/a)
            high = min(mp.mpf('.5'), (a*z+1)**(-1/a))
            assert z/d(p,l) < 1/(2*a)
        else:
            deficit = 1-l*z
            if l < 1:
                low = (deficit/2)**(1/l)
                high = min(mp.mpf('.5'), deficit**(1/l))
            else:
                low, high = deficit/(l+1), min(mp.mpf('.5'), deficit)
        assert low <= p*(1+mp.mpf('1e-38'))
        assert high >= p*(1-mp.mpf('1e-38'))
        count += 1
checks['inverse_brackets_and_joint_escape_bound'] = count

# Held-location equality: n=3, k=1, a=2, r=3/2.
a = mp.mpf(2)
r = 1+1/a
face = 2**(-a-2)*a**(-2*r)*2**(-r)
ratios = []
def inv(z, l):
    lo, hi = mp.mpf(0), mp.mpf('.5')
    # Fixed research precision loop only: this is not proposed fitter code.
    for _ in range(280):
        mid = (lo+hi)/2
        if -q(mid,l)>z:
            lo=mid
        else:
            hi=mid
    return (lo+hi)/2

for ss in ('.1', '.001', '1e-6', '1e-10'):
    s = mp.mpf(ss)
    likelihood = (2**(-a-2)/s)
    for dist in (mp.mpf(1), mp.mpf(2)):
        likelihood *= 1/(s*d(inv(dist/s,-a),-a))
    assert likelihood < face
    ratios.append(str(likelihood/face))
assert abs(mp.mpf(ratios[-1])-1)<mp.mpf('1e-8')
checks['held_location_equality_face_ratios'] = ratios

# Joint approach to the uniform contact corner, with t fixed at 2.
corner = []
for epss in ('.1', '.02', '.005'):
    eps = mp.mpf(epss)
    p = mp.exp(-2/eps)
    density = 1/d(p,1-eps)
    target = 1/(1+mp.exp(2))
    assert density <= mp.mpf('.5')
    corner.append(str(abs(density-target)))
assert mp.mpf(corner[-1]) < mp.mpf('1e-60')
checks['uniform_corner_errors'] = corner
print('Exact identities, envelopes, inverses and limits passed',flush=True)

# Independent numerical exploration of the unresolved free-location equality.
# lambda=-1 has an explicit rational inverse, avoiding the library entirely.
def logg_minus1(z):
    z = np.abs(np.asarray(z))
    p = 2/(z+2+np.hypot(z,2))
    v = 1-p
    return 2*np.log(p)+2*np.log(v)-np.log(p*p+v*v)

explorations = []
samples = [[0, 1], [0, 0, 1, 2], [0, 0, 1, 100],
           [0, 0, -1, 1], [0, 0, 0, 1, 2, 100]]
for index, raw in enumerate(samples):
    x = np.asarray(raw, dtype=float)
    values, counts = np.unique(x, return_counts=True)
    faces = []
    for t, k in zip(values, counts):
        if 2*k == len(x):
            faces.append(-3*k*np.log(2)-2*np.log(np.abs(x[x!=t]-t)).sum())
    best_face = max(faces)
    def negll(theta):
        mu, logsigma = theta
        return float(len(x)*logsigma-logg_minus1((x-mu)*np.exp(-logsigma)).sum())
    result = differential_evolution(negll, [(x.min()-1,x.max()+1),(-18,5)],
                                    seed=116170+index, maxiter=350, popsize=12,
                                    polish=True, tol=1e-10, workers=1)
    assert np.isfinite(result.fun) and abs(negll(result.x)-result.fun)<1e-10
    explorations.append(dict(sample=raw, best_finite_loglik=float(-result.fun),
                             face_loglik=float(best_face),
                             finite_minus_face=float(-result.fun-best_face),
                             numerical_only=True,
                             meaning='feasible lower witness; not an MLE or upper certificate',
                             proposal_converged=bool(result.success),
                             proposal_message=str(result.message),
                             evaluations=int(result.nfev),
                             search_box=[ [float(x.min()-1),float(x.max()+1)],[-18,5]]))
checks['equality_exploration_not_a_certificate'] = explorations

# Other equality exponents: numerical exploration, not an existence decision.
other = []
for index, (a, raw) in enumerate([(2.,[0,1,10]), (3.,[0,1,2,100]),
                                 (.5,[0,0,1]), (1/3,[0,0,0,1])]):
    x = np.asarray(raw,dtype=float)
    values, counts = np.unique(x,return_counts=True)
    r = 1+1/a
    faces = [-(a+2)*k*np.log(2)-r*(len(x)-k)*np.log(a)
             -r*np.log(np.abs(x[x!=t]-t)).sum()
             for t,k in zip(values,counts) if abs((len(x)-k)/a-k)<1e-12]
    def negll_other(theta):
        mu, logsigma = theta
        z = np.abs(x-mu)*np.exp(-logsigma)
        lo,hi = np.zeros(len(x)),np.full(len(x),.5)
        for _ in range(100):
            mid = (lo+hi)/2
            h = (mid**(-a)-(1-mid)**(-a))/a
            mask = h > z
            lo = np.where(mask,mid,lo)
            hi = np.where(mask,hi,mid)
        p = (lo+hi)/2
        return float(len(x)*logsigma+np.log(p**(-a-1)+(1-p)**(-a-1)).sum())
    result = differential_evolution(negll_other,[(x.min()-1,x.max()+1),(-12,5)],
                                    seed=116180+index,maxiter=180,popsize=10,
                                    polish=True,tol=1e-9,workers=1)
    assert np.isfinite(result.fun) and abs(negll_other(result.x)-result.fun)<1e-10
    other.append(dict(a=a,sample=raw,face_loglik=float(max(faces)),
                      finite_loglik=float(-result.fun),numerical_only=True,
                      meaning='feasible lower witness; not an MLE or upper certificate',
                      proposal_converged=bool(result.success),
                      proposal_message=str(result.message),
                      evaluations=int(result.nfev),
                      search_box=[[float(x.min()-1),float(x.max()+1)],[-12,5]]))
checks['other_equality_exploration_not_a_certificate'] = other

sources = ['univariateML-cpp/include/cppunivariate/distributions/tlambda/'+f
           for f in ('README.md','mle.hpp','dpqr.hpp','test.cpp','golden.json')]
manifest = {name:hashlib.sha256((ROOT/name).read_bytes()).hexdigest() for name in sources}
research_manifest = {name:hashlib.sha256((OUT/name).read_bytes()).hexdigest()
                     for name in ('check.py','proofs.md','subsets.md')}
record = dict(source_revision=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
              environment=dict(python=sys.version,platform=platform.platform(),
                               mpmath=mp.__version__,numpy=np.__version__,scipy=scipy.__version__,
                               decimal_precision=mp.mp.dps),checks=checks,source_sha256=manifest,
              research_sha256=research_manifest)
(OUT/'checks.json').write_text(json.dumps(record,indent=2)+'\n')
print(json.dumps(dict(status='passed', principal_points=grid_count,
                     inverse_points=count, output=str(OUT/'checks.json'))))
