"""Seam-coordinate research only: no output is a certified estimator.

Uses the retained cert samples and its conditional primal/dual solver.
No previous evidence is overwritten. Run through the local CPU slot lock.
"""
import csv
import hashlib
import heapq
import importlib.util
import json
from pathlib import Path
import platform
import sys

import mpmath as mp
import numpy as np
import scipy
from scipy.optimize import brentq
from scipy.special import logsumexp

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
PREV = HERE.parent / '2026-11-03-burr-cert-data'
spec = importlib.util.spec_from_file_location('previous', PREV / 'study.py')
previous = importlib.util.module_from_spec(spec)
spec.loader.exec_module(previous)
BUDGET = 512


def seam_fit(y, xi):
    n = len(y)
    def at_c(c):
        def score(eta):
            v = np.exp(c*y+eta)
            return np.sum((1-v)/(1+xi*v))
        eta = brentq(score, -c*max(y)-1, -c*min(y)+1,
                     xtol=2e-14, rtol=2e-14)
        v = np.exp(c*y+eta)
        return n/c+np.dot(y, (1-v)/(1+xi*v)), eta, v
    hi = 1.
    for _ in range(80):
        if at_c(hi)[0] < 0:
            break
        hi *= 2
    else:
        raise RuntimeError('seam c bracket budget')
    c = brentq(lambda c: at_c(c)[0], 1e-8, hi, xtol=2e-14, rtol=2e-14)
    _, eta, v = at_c(c)
    q = v if xi == 0 else (1+xi)*np.log1p(xi*v)/xi
    value = n*np.log(c)+n*eta+c*sum(y)-sum(q)
    r = np.column_stack((y, np.ones(n)))
    weight = (1+xi)*v/(1+xi*v)**2
    information = r.T @ (weight[:, None]*r)+np.diag([n/c**2, 0])
    mixed = r.T @ (v*(v-1)/(1+xi*v)**2)
    # Gauss-Legendre integrates the smooth derivative integrals on [0,v].
    nodes, weights = np.polynomial.legendre.leggauss(40)
    u = v[:, None]*(nodes+1)/2
    quadrature = v[:, None]*weights/2
    slope = -np.sum(quadrature*(1-u)/(1+xi*u)**2)
    direct = 2*np.sum(quadrature*u*(1-u)/(1+xi*u)**3)
    curvature = direct+mixed @ np.linalg.solve(information, mixed)
    return dict(c=c, eta=eta, v=v, value=value, slope=slope,
                curvature=curvature, information=information)


def tail_constants(y, w, radius, h):
    """Bounds in derivation.md, conditional on the exact Weibull fit."""
    n, c, v = len(y), w['c'], w['v']
    r = np.column_stack((y, np.ones(n)))
    norms = np.linalg.norm(r, axis=1)
    low, high = v*np.exp(-radius*norms), v*np.exp(radius*norms)
    weights = low/(1+h*high)**2
    matrix = r.T @ (weights[:, None]*r)+np.diag([n/(c+radius)**2, 0])
    mu = np.linalg.eigvalsh(matrix)[0]
    d = r.T @ (v*(v-1))
    # g_xi(theta_W) = xi*d - xi^2 sum v^2(v-1)/(1+xi*v)*r.
    D = np.linalg.norm(d)+h*np.sum(v*v*abs(v-1)*norms)
    a = np.minimum(v, 1)
    B0 = sum(a*a/2-a*a*a/3)
    # Retain the negative u>1 contribution using its largest denominator.
    B0 += sum(np.where(v > 1, (v*v/2-v**3/3-1/6)/(1+h*v)**3, 0))
    metric_D = np.sqrt(d @ np.linalg.solve(matrix,d))+h*np.sum(v*v*abs(v-1)*norms)/np.sqrt(mu)
    B = B0+metric_D*metric_D/2
    return dict(radius=radius, h=h, mu=mu, D=D, B0=B0, B=B,
                containment_ratio=h*D/(mu*radius))


def signed_tail(y, w):
    A = np.dot(w['v'], w['v'])/2-len(y)
    if A >= 0:
        return None
    candidates = []
    for fraction in [.005, .02, .1, .3, .7]:
        radius = fraction*min(w['c'], 1)
        h = min(1., (len(y)-1)/2)
        for _ in range(100):
            bound = tail_constants(y, w, radius, h)
            if bound['containment_ratio'] <= .5 and h*bound['B'] <= -A/2:
                candidates.append(dict(A=A, **bound))
                break
            h /= 2
        else:
            raise RuntimeError('signed-tail budget')
    return max(candidates, key=lambda a: a['h'])


def enclose(y, w, tail):
    """Replay k-chords with signed seam exclusion, 512 profiles total."""
    n = len(y)
    m = sum(y == min(y))
    p = n*np.log(n/sum(y-min(y)))-n
    eps = 1e-6*n
    cache = {}
    def get(k):
        k = float(k)
        if k not in cache:
            cache[k] = previous.profile(y, k)
        return cache[k]
    for k in np.geomspace(m/(n-m), max(10000, 4*n), 41):
        get(k)
    best = max(p, w['value'], max(a['lower'] for a in cache.values()))
    K = float(2*n)
    for _ in range(80):
        tail_upper = w['value']+n*n*(K+1)/(2*(K+1-n)**2)
        if tail_upper <= best+eps:
            break
        K *= 2
    else:
        raise RuntimeError('unsigned-tail budget')
    if tail and 1/tail['h'] < K:
        K, tail_upper = 1/tail['h'], w['value']
    knots = np.geomspace(m/(n-m), K, max(2, int(np.ceil(np.log(K/(m/(n-m)))/.6))+1))
    nodes = [get(k) for k in knots]
    best = max(best, max(a['lower'] for a in nodes))
    heap = []
    serial = 0
    def push(a, b):
        nonlocal serial
        serial += 1
        heapq.heappush(heap, (-previous.interval_upper(a,b,n), serial, a, b))
    for a, b in zip(nodes[:-1], nodes[1:]):
        push(a,b)
    while -heap[0][0] > best+eps and len(cache) < BUDGET:
        _, _, a, b = heapq.heappop(heap)
        mid = get(np.sqrt(a['k']*b['k']))
        best = max(best, mid['lower'])
        push(a,mid)
        push(mid,b)
    gap = max(p, tail_upper, -heap[0][0])-best
    return dict(gap=gap, profiles=len(cache), K=K,
                outcome='value-gap-only' if gap <= eps*(1+1e-8) else 'subdivision-budget',
                finite_lower=max(a['lower'] for a in cache.values()))


def high_precision_checks():
    """Independent 70-digit identities and sign checks, not interval arithmetic."""
    mp.mp.dps = 70
    rng = np.random.default_rng(13803)
    largest_excess = mp.ninf
    for _ in range(200):
        v = mp.mpf(str(10**rng.uniform(-5, 3)))
        xi = mp.mpf(str(10**rng.uniform(-6, 1)))
        q = lambda z: (1+z)*mp.log1p(z*v)/z
        actual = -q(xi)+v
        A = v*v/2-v
        a = min(v, 1)
        B0 = a*a/2-a*a*a/3
        excess = actual-A*xi-B0*xi*xi
        assert excess < mp.mpf('1e-55')
        largest_excess = max(largest_excess, excess)
        d2 = -mp.diff(q, xi, 2)
        integral = 2*mp.quad(lambda u: u*(1-u)/(1+xi*u)**3, [0, min(v,1), v])
        assert abs(d2-integral) < mp.mpf('1e-50')
        assert d2 <= 2*B0+mp.mpf('1e-55')
    # Re-run the old exact rational counterexample certificates, read-only.
    sys.path.insert(0, str(HERE.parent/'2026-11-03-burr-closure-data'))
    from cert import certificates
    exact = certificates()
    return dict(precision=70, trials=200, tolerance='1e-55',
                largest_taylor_excess=str(largest_excess),
                exact_counterexample_certificate_groups=len(exact))


def main():
    samples = json.loads((PREV/'samples.json').read_text())
    closure = json.loads((HERE.parent/'2026-11-03-burr-closure-data/summary.json').read_text())
    for name, info in closure['samples'].items():
        samples['closure-'+name] = np.exp(np.array(info['log_data'], dtype=float)).tolist()
    u = np.array([0,5,7,8,8,15,15])/15
    v = np.array([0,0,0,0,0,1,1])
    for label, t in [('left', .0439243843), ('right', .0439243846),
                     ('numerical-zero', float(closure['zero_slope']['t_numerical']))]:
        samples['closure-zero-'+label] = np.exp((1-t)*u+t*v).tolist()
    for t in [.23826, .23827]:
        samples[f'closure-equal-bracket-{t}'] = np.exp([0,t,7/15,8/15,8/15,1,1]).tolist()
    for q in [0., .5, 1.]:
        samples[f'closure-tie-family-q{q}'] = np.exp([0,.23826*q,7*q/15,8*q/15,8*q/15,1,1]).tolist()
    (HERE/'samples.json').write_text(json.dumps(samples,separators=(',',':'))+'\n')
    rows = []
    checks = []
    roots = []
    for name, x in samples.items():
        logx = np.log(np.array(x))
        unit = np.std(logx)
        y = (logx-np.mean(logx))/unit
        w = seam_fit(y, 0.)
        tail = signed_tail(y,w)
        result = enclose(y,w,tail)
        row = dict(sample=name, n=len(y), A=np.dot(w['v'], w['v'])/2-len(y),
                   seam_curvature=w['curvature'], signed_h=tail['h'] if tail else 0.,
                   signed_mu=tail['mu'] if tail else 0.,
                   signed_B=tail['B'] if tail else 0.,
                   signed_radius=tail['radius'] if tail else 0.,
                   signed_D=tail['D'] if tail else 0.,
                   containment_ratio=tail['containment_ratio'] if tail else 0., **result)
        rows.append(row)
        # Check the actual profile within the proved nuisance neighborhood.
        if tail:
            for f in [.125, .5, 1.]:
                xi = f*tail['h']
                fit = seam_fit(y,xi)
                displacement = np.linalg.norm([fit['c']-w['c'], fit['eta']-w['eta']])
                upper = row['A']*xi+tail['B']*xi*xi
                excess = fit['value']-w['value']-upper
                assert displacement < tail['radius']
                assert excess < 2e-10*len(y)
                checks.append(dict(sample=name, xi=xi, displacement=displacement,
                                   radius=tail['radius'], upper_excess=excess))
        # Numerically isolate score crossings on a finite reconnaissance mesh.
        # This is deliberately NOT a root isolation or uniqueness certificate.
        kk = np.geomspace(sum(y == min(y))/(len(y)-sum(y == min(y))), min(result['K'],1e5),81)
        def score(k):
            fit = previous.profile(y,k)
            z = fit['c']*y+fit['eta']-np.log(k)
            return len(y)/k-sum(np.logaddexp(0,z))
        ss = [score(k) for k in kk]
        for a,b,sa,sb in zip(kk[:-1],kk[1:],ss[:-1],ss[1:]):
            if sa*sb < 0:
                k = brentq(score,a,b,xtol=1e-12,rtol=1e-13)
                fit = previous.profile(y,k)
                c = fit['c']/unit
                log_s = np.mean(logx)+unit*(np.log(k)-fit['eta'])/fit['c']
                curvature = seam_fit(y,1/k)['curvature'] if k > .1 else float('nan')
                roots.append(dict(sample=name,k=k,c=c,scale=np.exp(log_s),
                                  type='maximum' if sa>0 else 'minimum',
                                  seam_curvature=curvature, score=score(k)))
        print(name, result['outcome'], result['profiles'], 'h=',row['signed_h'], flush=True)
    for filename,data in [('diagnostics.csv',rows),('tail-checks.csv',checks),('roots.csv',roots)]:
        with (HERE/filename).open('w') as f:
            writer=csv.DictWriter(f,fieldnames=list(data[0])); writer.writeheader(); writer.writerows(data)
    golden=json.loads((ROOT/'univariateML-cpp/include/cppunivariate/distributions/burr/golden.json').read_text())
    golden_root=next(r for r in roots if r['sample']=='golden' and r['type']=='maximum')
    errors={key:abs(golden_root[col]/golden['estimates'][key]-1)
            for key,col in [('shape1','k'),('shape2','c'),('scale','scale')]}
    summary=dict(source_revision='65866760db7f0335c7bf819f400a1c35a04a39a1',
                 script_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                 environment=dict(python=platform.python_version(),numpy=np.__version__,scipy=scipy.__version__,
                                  mpmath=mp.__version__,platform=platform.platform()),
                 samples=len(rows), profiles=sum(r['profiles'] for r in rows),
                 outcomes={key:sum(r['outcome']==key for r in rows)
                           for key in ['value-gap-only','subdivision-budget']},
                 signed_tail_samples=sum(r['signed_h']>0 for r in rows),
                 tail_checks=len(checks), golden_relative_coefficient_errors=errors,
                 golden_numerical_root=golden_root, high_precision=high_precision_checks(),
                 note='Conditional exact-arithmetic theorem; numerical constants and root reconnaissance are not production certificates.')
    (HERE/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
    print(json.dumps(summary,indent=2))


if __name__ == '__main__':
    main()
