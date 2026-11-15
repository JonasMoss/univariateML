"""Mapped tie3 answers and their representability, per family (mpmath, 80 digits).

Reads the stored snapshots, maps the tie3 coefficients through the declared
transformation (the one sending [q50, q50, q75] onto [c, c, nextafter(c)]),
rounds them to binary64 and measures the likelihood lost by rounding, using the
exact binary64 near-constant data.
"""
import sys, math, itertools, json
sys.path.insert(0, 'dev/validation')
import strict_common as sc
from mpmath import mp, mpf, log, exp, sqrt, pi, erf, erfc
mp.dps = 80
D = 'dev/research/evaluations/2026-11-15-fit-snapshot-data/'
M = lambda v: mpf(float(v))

def tonum(x):
    return sc.number(x) if isinstance(x, str) else x

# ---- log densities (sum over data), coefficients are mp numbers ----
def ll_pareto(x, a, b):  return sum(log(a) + a*log(b) - (a+1)*log(v) for v in x) if all(v >= b for v in x) else -mp.inf
def ll_invweibull(x, scale, shape): return sum(log(shape) + shape*log(scale) - (shape+1)*log(v) - (v/scale)**(-shape) for v in x)
def ll_power(x, alpha, beta): return sum(log(beta) - beta*log(alpha) + (beta-1)*log(v) for v in x) if all(v <= alpha for v in x) else -mp.inf
def ll_hnorm(x, mu, sigma): return sum(log(2/pi)/2 - log(sigma) - (v-mu)**2/(2*sigma**2) for v in x) if all(v >= mu for v in x) else -mp.inf
def ll_norm(x, mean, sd): return sum(-log(sd) - log(2*pi)/2 - (v-mean)**2/(2*sd**2) for v in x)
def ll_texp(x, mu, rate): return sum(log(rate) - rate*(v-mu) for v in x) if all(v >= mu for v in x) else -mp.inf
def ll_huber(x, mu, sigma, eps):
    k = sqrt(2*pi)*erf(eps/sqrt(2)) + 2*exp(-eps**2/2)/eps
    t = 0
    for v in x:
        z = abs((v-mu)/sigma)
        t += -log(sigma) - log(k) - (z**2/2 if z <= eps else eps*z - eps**2/2)
    return t
def ll_triang(x, a, b, c):
    t = 0
    for v in x:
        if v < a or v > b: return -mp.inf
        if v < c: t += log(2) - log(b-a) + log((v-a)/(c-a))
        elif v > c: t += log(2) - log(b-a) + log((b-v)/(b-c))
        else:
            if c == a or c == b: t += log(2) - log(b-a)
            else: t += log(2) - log(b-a)
    return t
def ll_fpareto(x, shape2, scale, shape1, shape3):   # min = 0; p = shape3, q = shape1
    p, q, a, b = mpf(shape3), mpf(shape1), shape2, scale
    from mpmath import beta as Beta
    return sum(log(a) - log(v) + a*p*log(v/b) - (p+q)*log(1+(v/b)**a) - log(Beta(p, q)) for v in x)

def maps(group, A, B):
    """coefficient maps by model: name -> (names, function)"""
    if group == 'affine':
        return dict(loc=lambda m: A + B*m, scale=lambda s: B*s, rate=lambda r: r/B)
    return dict(minpow=lambda m: A*m**B, scalepow=lambda s: A*s**B, shapepow=lambda s: s/B)

def ulp_neighbours(v):
    f = float(v)
    lo = f if mpf(f) <= v else math.nextafter(f, -math.inf)
    hi = f if mpf(f) >= v else math.nextafter(f, math.inf)
    return mpf(lo), mpf(hi)

def analyse(name, group, data_nc, data3, row_nc, row3, A, B, tool):
    """tool: (model, names, mapped(list of mp), ll(list of mp)->mp, free names)"""
    model, names, theta, ll = tool
    exact = ll(theta)
    rnd = [mpf(float(t)) for t in theta]
    rel = [abs((r-t)/t) if t != 0 else abs(r-t) for r, t in zip(rnd, theta)]
    nearest_loss = exact - ll(rnd)
    best = -mp.inf; best_combo = None
    for combo in itertools.product(*[ulp_neighbours(t) for t in theta]):
        v = ll(list(combo))
        if v > best: best, best_combo = v, list(combo)
    best_loss = exact - best
    normal = all(math.isfinite(float(t)) and abs(float(t)) >= 2.2250738585072014e-308 for t in theta)
    return dict(model=model, names=names, mapped=[mp.nstr(t, 20) for t in theta],
                rounded=[repr(float(t)) for t in theta], max_rel_round=mp.nstr(max(rel), 3),
                finite_normal=normal, ll_exact=mp.nstr(exact, 15),
                loss_nearest=mp.nstr(nearest_loss, 6), loss_best_neighbours=mp.nstr(best_loss, 6),
                best_neighbour_coefficients=[repr(float(t)) for t in best_combo], best_neighbour_ll=mp.nstr(best, 15))

FAM = {
 'burr':       ('power-scale', 'pareto',      ['shape','min'],     lambda r,m: [r['a']/m['B'], m['A']*r['b']**m['B']], ll_pareto),
 'pareto4':    ('power-scale', 'pareto',      ['shape','min'],     lambda r,m: [r['a']/m['B'], m['A']*r['b']**m['B']], ll_pareto),
 'gengamma':   ('power-scale', 'pareto',      ['shape','min'],     lambda r,m: [r['a']/m['B'], m['A']*r['b']**m['B']], ll_pareto),
 'invburr':    ('power-scale', 'invweibull',  ['scale','shape'],   lambda r,m: [m['A']*r['scale']**m['B'], r['shape']/m['B']], ll_invweibull),
 'trgamma':    ('power-scale', 'power',       ['alpha','beta'],    lambda r,m: [m['A']*r['alpha']**m['B'], r['beta']/m['B']], ll_power),
 'sn':         ('affine',      'hnorm',       ['mu','sigma'],      lambda r,m: [m['A']+m['B']*r['mu'], m['B']*r['sigma']], ll_hnorm),
 'std':        ('affine',      'norm',        ['mean','sd'],       lambda r,m: [m['A']+m['B']*r['mean'], m['B']*r['sd']], ll_norm),
 'emg':        ('affine',      'translated-exp', ['mu','rate'],    lambda r,m: [m['A']+m['B']*r['mu'], r['rate']/m['B']], ll_texp),
 'fpareto':    ('power-scale', 'fpareto',     ['shape2','scale'],  lambda r,m: [r['shape2']/m['B'], m['A']*r['scale']**m['B']], None),
 'huber':      ('affine',      'huber',       ['mu','sigma'],      lambda r,m: [m['A']+m['B']*r['mu'], m['B']*r['sigma']], None),
 'triang':     ('affine',      'triang',      ['a','b','c'],       lambda r,m: [m['A']+m['B']*r['a'], m['A']+m['B']*r['b'], m['A']+m['B']*r['c']], ll_triang),
}

EXACT = {'burr': {1:'min'}, 'pareto4': {1:'min'}, 'gengamma': {1:'min'}, 'trgamma': {0:'max'},
         'sn': {0:'min'}, 'emg': {0:'min'}, 'triang': {0:'min', 2:'min'}}
def main():
    out = {}
    for mode in ('native', 'double'):
        rows = sc.load(D + mode + '.jsonl.gz')
        for fam, (group, model, names, mapfn, llf) in FAM.items():
            r3, rn = rows[(fam,'class','tie3')], rows[(fam,'class','near-constant')]
            x3 = [M(v) for v in r3['data']]; xn = [M(v) for v in rn['data']]
            q50, q75 = x3[0], x3[2]; c, c1 = xn[0], xn[2]
            if group == 'affine':
                B = (c1-c)/(q75-q50); A = c - B*q50
                chk = [A+B*q50-c, A+B*q75-c1]
            else:
                B = log(c1/c)/log(q75/q50); A = c/q50**B
                chk = [A*q50**B-c, A*q75**B-c1]
            res = r3['result']
            coef = {k: M(tonum(v)) for k, v in (res.get('limit_estimates') or {}).items()}
            coef.update({k: M(tonum(v)) for k, v in (res.get('estimates') or {}).items()})
            mp_ = dict(A=A, B=B)
            theta = mapfn(coef, mp_)
            # endpoints of limit models and triang are data values: use them exactly
            for i in EXACT.get(fam, {}):
                theta[i] = (min if EXACT[fam][i] == 'min' else max)(xn)
            held = {k: M(v) for k, v in (r3.get('held') or {}).items()}
            if fam == 'fpareto':
                f = lambda t, h=held: ll_fpareto(xn, t[0], t[1], h['shape1'], h['shape3'])
            elif fam == 'huber':
                f = lambda t, h=held: ll_huber(xn, t[0], t[1], h['epsilon'])
            else:
                f = lambda t, ll=llf: ll(xn, *t)
            info = analyse(fam, group, xn, x3, rn, r3, A, B, (model, names, theta, f))
            info.update(group=group, mode=mode, c=repr(float(c)), B=mp.nstr(B, 12), A=mp.nstr(A, 20),
                        map_check=[mp.nstr(v, 3) for v in chk], tie3_kind=res['kind'],
                        nc_kind=rn['result']['kind'], nc_reason=rn['result'].get('reason'),
                        tie3_coef={k: mp.nstr(v, 17) for k, v in coef.items()})
            out[mode+':'+fam] = info
            print(json.dumps({fam+'/'+mode: info}))
    json.dump(out, open('dev/research/evaluations/2026-11-15-equivariance-data/mapped.json', 'w'), indent=1)
main()
