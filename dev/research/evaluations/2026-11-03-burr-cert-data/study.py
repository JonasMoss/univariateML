"""Reproducible research diagnostics; never returns a certified MLE.

Run run.sh from the repository root. Budgets produce explicit diagnostic
outcomes; endpoint profiles use bracketed roots of a strictly concave fit.
"""
import csv
import heapq
import json
from pathlib import Path
import subprocess

import mpmath as mp
import numpy as np
from scipy.optimize import brentq
from scipy.special import expit, logsumexp

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
SEED = 138
BUDGET = 512
REL_GAP = 1e-6  # n log-likelihood units; diagnostics only
DUAL_PAD = 1e-9  # n log-likelihood units, empirical arithmetic allowance


def profile(y, k):
    """Fixed k primal/dual bracket; y is centered and standardized."""
    n = len(y)
    def fit_c(c):
        def residual(eta):
            z = c*y + eta - np.log(k)
            return (np.sum((k+1)*expit(-z)-k) if k < 1 else
                    np.sum(1-(k+1)*expit(z)))
        eta = brentq(residual, -c*max(y)-1, -c*min(y)+1,
                     xtol=2e-13, rtol=2e-14)
        z = c*y+eta-np.log(k)
        score_i = (k+1)*expit(-z)-k if k < 1 else 1-(k+1)*expit(z)
        return n/c+np.dot(y, score_i), z, eta
    lo, hi = 1e-8, 1.0
    for _ in range(80):
        if fit_c(hi)[0] < 0:
            break
        hi *= 2
    else:
        raise RuntimeError("c bracket budget exhausted")
    if fit_c(lo)[0] <= 0:
        raise RuntimeError("lower c bracket failed")
    c = brentq(lambda c: fit_c(c)[0], lo, hi, xtol=2e-13, rtol=2e-14)
    _, z, eta = fit_c(c)
    # Stable representations of z-(k+1)sp(z).
    terms = -k*z-(k+1)*np.logaddexp(0, -z) if k < 1 else (
        z-(k+1)*np.logaddexp(0, z))
    primal = n*np.log(k)+n*np.log(c)+np.sum(terms)
    # Exact feasibility is a condition of the theorem. Record the finite
    # precision repair residual rather than declaring it to be exact.
    ld = np.longdouble
    p = expit(z).astype(ld)
    target = ld(n)/(ld(k)+1)
    j = int(np.argmax(p*(1-p)))
    p[j] = target - np.sum(np.delete(p, j), dtype=ld)
    if np.any(p < 0) or np.any(p > 1):
        raise RuntimeError("dual vector is outside its domain")
    residual = abs(np.sum(p, dtype=ld)-target)
    a = np.sum(((ld(k)+1)*p-1)*y.astype(ld), dtype=ld)
    if a <= 0:
        raise RuntimeError("dual A is nonpositive")
    interior = p[(p>0)&(p<1)]
    entropy = -np.sum(interior*np.log(interior)+(1-interior)*np.log1p(-interior), dtype=ld)
    dual = ld(n)*np.log(ld(k))+ld(n)*np.log(ld(n)/a)-n-(ld(k)+1)*entropy
    raw_gap = float(dual)-primal
    if raw_gap < -2e-8*n:
        raise RuntimeError(f"dual violation {raw_gap}")
    upper = max(float(dual), primal)+DUAL_PAD*n
    return dict(k=k, lower=float(primal), upper=upper, c=float(c),
                eta=float(eta), raw_gap=raw_gap, residual=float(residual))


def weibull(y):
    n = len(y)
    def score(c):
        w = np.exp(c*y-logsumexp(c*y))
        return n/c+sum(y)-n*np.dot(w, y)
    hi = 1.0
    while score(hi) > 0:
        hi *= 2
    c = brentq(score, 1e-8, hi, xtol=1e-13)
    t = n*np.exp(c*y-logsumexp(c*y))
    value = n*np.log(c)+sum(np.log(t))-n
    return float(value), float(np.dot(t,t)/2-n), c


def interval_upper(a, b, n):
    ga, gb = a['upper']-n*np.log(a['k']), b['upper']-n*np.log(b['k'])
    slope = (gb-ga)/(b['k']-a['k'])
    k = np.clip(-n/slope, a['k'], b['k']) if slope < 0 else b['k']
    return float(n*np.log(k)+ga+slope*(k-a['k']))


def diagnose(x):
    y0 = np.log(x)
    center, unit = float(np.mean(y0)), float(np.std(y0))
    y = (y0-center)/unit
    n = len(y)
    jacobian = -sum(y0)-n*np.log(unit)
    m = sum(x == min(x))
    p = n*np.log(n/sum(y-min(y)))-n
    w, slope, wc = weibull(y)
    cache = {}
    def get(k):
        if k not in cache:
            cache[k] = profile(y, k)
        return cache[k]
    # A finite logarithmic reconnaissance, not an optimizer at a cap.
    for k in np.geomspace(m/(n-m), max(10000, 4*n), 41):
        get(float(k))
    best = max(p, w, max(a['lower'] for a in cache.values()))
    eps = REL_GAP*n
    # Large-k bound determines K, rather than treating a cap as an estimate.
    K = float(2*n)
    for _ in range(80):
        tail = w+n*n*(K+1)/(2*(K+1-n)**2)
        if tail <= best+eps:
            break
        K *= 2
    else:
        raise RuntimeError("tail cutoff budget exhausted")
    knots = np.geomspace(m/(n-m), K, max(2, int(np.ceil(np.log(K/(m/(n-m)))/.6))+1))
    nodes = [get(float(k)) for k in knots]
    best = max(best, max(a['lower'] for a in nodes))
    heap = []
    serial = 0
    def push(a, b):
        nonlocal serial
        serial += 1
        heapq.heappush(heap, (-interval_upper(a,b,n), serial, a, b))
    for a, b in zip(nodes[:-1], nodes[1:]):
        push(a,b)
    while heap and -heap[0][0] > best+eps and len(cache) < BUDGET:
        _, _, a, b = heapq.heappop(heap)
        mid = get(float(np.sqrt(a['k']*b['k'])))
        best = max(best, mid['lower'])
        push(a,mid)
        push(mid,b)
    global_upper = max(p, tail, -heap[0][0])
    candidate = max(cache.values(), key=lambda a: a['lower'])
    return dict(n=n, pareto=p+jacobian, weibull=w+jacobian,
                best_lower=best+jacobian, finite_lower=candidate['lower']+jacobian,
                finite_k=candidate['k'], global_upper=global_upper+jacobian,
                gap=global_upper-best, outcome=('value-gap-only' if global_upper-best <= eps*(1+1e-8)
                                               else 'subdivision-budget'),
                pareto_exclusion_margin=p-max(tail,-heap[0][0]),
                profiles=len(cache), K=K, weibull_slope=slope,
                max_dual_gap=max(a['raw_gap'] for a in cache.values()),
                min_dual_gap=min(a['raw_gap'] for a in cache.values()),
                max_dual_residual=max(a['residual'] for a in cache.values()),
                # Preserve the best fixed-k fit for independent re-evaluation.
                finite_c=candidate['c']/unit,
                finite_log_s=center+unit*(np.log(candidate['k'])-candidate['eta'])/candidate['c'])


def checks():
    mp.mp.dps = 70
    rng = np.random.default_rng(SEED)
    worst = mp.mpf(0)
    for _ in range(300):
        n = int(rng.integers(2,30))
        # Include exact tied minima; positive nonconstant sample on log scale.
        y = [mp.mpf(0)]*int(rng.integers(1,n))
        y += [mp.mpf(str(v)) for v in rng.uniform(.01,5,n-len(y))]
        m, D = y.count(0), sum(y)
        k = mp.mpf(str(rng.uniform(.001,.999)))*m/(n-m)
        q = mp.mpf(str(rng.uniform(.01,10)))
        t = mp.mpf(str(rng.uniform(-15,15)))
        ell = n*mp.log(q)-q*D-n*k*t-(k+1)*sum(mp.log1p(mp.exp(-t-q*d/k)) for d in y)
        P = n*mp.log(n/D)-n
        a, b = n*k, m*(k+1)
        C = a*mp.log(a/b)+(b-a)*mp.log1p(-a/b)
        assert ell <= P+C+mp.mpf('1e-55')
        # Construct exact feasible dual vectors from arbitrary z values by
        # solving the intercept constraint at high precision.
        k = mp.mpf(str(10**rng.uniform(-2,5)))
        c = mp.mpf(str(rng.uniform(.1,5)))
        lo, hi = -mp.log(k)-c*max(y)-1, -mp.log(k)-c*min(y)+1
        for _ in range(250):
            b0=(lo+hi)/2
            if sum(1/(1+mp.exp(-c*v-b0)) for v in y)>n/(k+1): hi=b0
            else: lo=b0
        z = [c*v+(lo+hi)/2 for v in y]
        p = [1/(1+mp.exp(-v)) for v in z]
        p[-1] = n/(k+1)-sum(p[:-1])
        A = sum(((k+1)*p0-1)*v for p0,v in zip(p,y))
        assert A>0 and all(0<p0<1 for p0 in p)
        H = -sum(p0*mp.log(p0)+(1-p0)*mp.log1p(-p0) for p0 in p)
        U = n*mp.log(k)+n*mp.log(n/A)-n-(k+1)*H
        # Arbitrary primal, different from the dual's generating c/intercept.
        cc = c*mp.mpf(str(rng.uniform(.2,3)))
        bb = (lo+hi)/2+mp.mpf(str(rng.uniform(-3,3)))
        zz = [cc*v+bb for v in y]
        L = n*mp.log(k)+n*mp.log(cc)+sum(zz)-(k+1)*sum(mp.log1p(mp.exp(v)) for v in zz)
        worst = max(worst,L-U)
        assert L <= U+mp.mpf('1e-55')
        # Scalar inequality underpinning the uniform Weibull tail bound.
        u = mp.mpf(str(10**rng.uniform(-8,5)))
        assert mp.log1p(u)>=u-u*u/2-mp.mpf('1e-55')
    return dict(trials=300, precision=70, tolerance='1e-55', worst_dual_violation=str(worst))


def main():
    rng = np.random.default_rng(SEED)
    golden = json.loads((ROOT/'univariateML-cpp/include/cppunivariate/distributions/burr/golden.json').read_text())
    samples = [('golden',np.array(golden['data']))]
    for rep in range(5):
        e = rng.exponential(size=50)
        for k,c in [(1,2),(.08,2),(1000,2)]:
            # Unit Weibull scale in the k->infinity regime.
            lx = (np.log(np.expm1(e/k))+np.log(k))/c
            samples.append((f'burr-k{k}-r{rep}',np.exp(lx)))
        samples.extend([(f'weibull-r{rep}',np.sqrt(e)),
                        (f'pareto-r{rep}',np.exp(e/2)),
                        (f'lognormal-r{rep}',rng.lognormal(0,1,50))])
    u = (np.arange(200)+.5)/200
    samples += [('weibull-quantiles', np.sqrt(-np.log1p(-u))),
                ('pareto-quantiles', (1-u)**(-.5)),
                ('tied-minimum', np.array([1.,1.,1.,2.,4.,8.])),
                ('two-observations', np.array([1.,2.])),
                ('nearly-constant', np.exp(np.linspace(-1e-6,1e-6,20)))]
    # Same uniforms approach the Pareto face, avoiding enormous observations:
    # c=2/k, s=1, log X=(k/2)log(expm1(E/k)).
    e = rng.exponential(size=50)
    for k in [1e-2,1e-4,1e-6]:
        lx = (e+k*np.log1p(-np.exp(-e/k)))/2
        samples.append((f'near-pareto-k{k}',np.exp(lx)))
    (HERE/'samples.json').write_text(json.dumps({name:x.tolist() for name,x in samples},separators=(',',':'))+'\n')
    payload=''.join(str(len(x))+' '+' '.join(format(v,'.17g') for v in x)+'\n' for _,x in samples)
    proc=subprocess.run([str(ROOT/'build/scratch/burr-cert-probe')],input=payload,text=True,capture_output=True,check=True)
    existing=[line.split() for line in proc.stdout.splitlines()]
    assert len(existing)==len(samples)
    rows=[]
    for (name,x),r in zip(samples,existing):
        row=dict(sample=name,**diagnose(x))
        row.update(existing_status=int(r[0]),existing_k=float(r[1]),existing_c=float(r[2]),
                   existing_scale=float(r[3]),existing_logLik=float(r[4]))
        rows.append(row)
        print(name,row['outcome'],row['profiles'],format(row['gap'],'.5g'),flush=True)
    with (HERE/'diagnostics.csv').open('w') as f:
        writer=csv.DictWriter(f,fieldnames=list(rows[0])); writer.writeheader(); writer.writerows(rows)
    # Independent 70-digit evaluations of the retained finite candidates,
    # and additional midpoint/chord checks, including the near-limit samples.
    mp.mp.dps=70
    verification=[]
    for (name,x),row in zip(samples,rows):
        xx=[mp.mpf(float(v)) for v in x]
        k=mp.mpf(row['finite_k']); c=mp.mpf(row['finite_c']); ls=mp.mpf(row['finite_log_s'])
        yy=[mp.log(v) for v in xx]
        zz=[c*(v-ls) for v in yy]
        ll=len(x)*mp.log(k*c)-sum(yy)+sum(zz)-(k+1)*sum(mp.log1p(mp.exp(v)) for v in zz)
        error=abs(float(ll)-row['finite_lower'])
        assert error<2e-6*len(x)
        yn=(np.log(x)-np.mean(np.log(x)))/np.std(np.log(x))
        a=profile(yn, .4); b=profile(yn, 4.); mid=profile(yn, 1.2)
        upper=interval_upper(a,b,len(x))
        assert mid['lower']<=upper+1e-8*len(x)
        # Direct tail check at a very large k against the numerical Weibull
        # profile, independent of the production fitter's boundary decision.
        K=float(1000*len(x)); large=profile(yn,K)
        W,_,_=weibull(yn)
        E=len(x)**2*(K+1)/(2*(K+1-len(x))**2)
        assert large['lower']<=W+E+1e-8*len(x)
        verification.append(dict(sample=name, mp_logLik=str(ll), error=error,
                                 chord_slack=upper-mid['lower'], tail_slack=W+E-large['lower']))
    (HERE/'verification.json').write_text(json.dumps(verification,indent=2)+'\n')
    summary=dict(seed=SEED, samples=len(rows), profile_budget=BUDGET,
                 relative_value_gap=REL_GAP, dual_pad=DUAL_PAD,
                 outcomes={key:sum(r['outcome']==key for r in rows) for key in ['value-gap-only','subdivision-budget']},
                 high_precision=checks(),
                 total_profiles=sum(r['profiles'] for r in rows),
                 max_dual_residual=max(r['max_dual_residual'] for r in rows),
                 max_existing_shortfall=max(r['best_lower']-r['existing_logLik'] for r in rows),
                 max_independent_logLik_error=max(r['error'] for r in verification),
                 pareto_margin_over_1e_8_n=int(sum(r['pareto_exclusion_margin']>1e-8*r['n'] for r in rows)),
                 note='No output is a certified MLE or a certified winning face.')
    (HERE/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
    print(json.dumps(summary,indent=2))


if __name__=='__main__':
    main()
