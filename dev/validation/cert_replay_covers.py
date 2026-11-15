"""Certificate replay, mechanisms E and F: Lomax transfers and one-dimensional covers (step 8d, TASK-214.34).

Companion of cert-replay.py (design: dev/design/certificate-replay.md; report:
dev/research/evaluations/2026-11-15-replay-covers-report.md). Registered into
cert-replay.py's REPLAYS; every function has the signature
replay(record, data, budget) -> (comparisons, why_none) of the prototype.

Code read at master b80a1831 (package code identical to the commit of the
stored baselines). Claimed widths and margins are listed in CLAIMS with the
source line they were read from; a change to a certificate must update this
module in the same commit. The comparison names do not collide with the ones of
the other modules (root, restore, moment, support, unique, face, gain, global,
concave, dispersion, multiplicity, quartic, location): the Lomax transfers reuse
the Lomax names because pareto2 and invpareto are replayed by no other module
(support is the exact predicate for pareto2 samples with a value at min; faces.py
does not replay pareto2).

Transfers (re-use of cert-replay.py's replay_lomax, bound at registration):

  pareto2    shape = kappa, scale = 1/lambda on y = x - min (binary64, as the core subtracts);
             shape held is the held-kappa root of the Lomax replay, scale held is closed form;
             samples with a value equal to min take pareto2's own branch (support); the
             exponential limit is mapped to limit(true): comparison 'mapping' (information)
  invpareto  shape = kappa, scale = lambda on y = 1/x (binary64, as the core divides);
             logLik differs from the Lomax value by the Jacobian -2 sum log x (double column only)
  both       coefficient: log(kappa_hat) + log A(z) within the certificate's 1e-8 over the root enclosure
             (the Lomax prototype does not check the shape coefficient)

One-dimensional covers. A generic interval branch and bound (cover) closes a cell by an upper
bound of the value, by a one-signed derivative enclosure, or by two tangent lines; it never
uses a dossier bound that is not coded here as an interval:

  cauchy     scale held   bracket, curvature, cover (+ tie, n = 2)   interval
  gpd        joint, scale held   bracket, curvature, gain, cover, coefficient   interval
  foldnorm   joint        bracket, cover (profile in a = mu/sigma^2)     interval
  fatigue    shift held, scale free or alpha held   bracket, cover   interval
  skellam    both rates free      cover (profile on the mean line, Bessel by mpmath)   numerical

Every comparison reports a verdict (holds, fails, open) as in the prototype. Results
are cached in a gzipped JSON-lines file (CERT_REPLAY_COVERS_CACHE, default
/tmp/cert-replay-covers-cache.jsonl.gz); a family's entry in VERSIONS goes up when its replay changes.
"""
import collections, heapq, math
from fractions import Fraction
import mpmath as mp
from strict_common import number

iv = mp.iv
EPS = 2.0**-52
TOL = 1e-10
CLAIMS = {}      # family -> {subset: (claim text, source)}; written to the records
LOMAX = None     # cert-replay.py's replay_lomax, bound at registration
LOMAX_PROFILE = None   # cert-replay.py's LomaxProfile, bound at registration


def hexed(v): return float(v).hex()

def comparison(name, claim, verdict, hp, double=None, margin=None, rigor='interval', note='', info=None):
    return dict(name=name, claim=claim, verdict=verdict, hp=hp, double=double, margin=margin, rigor=rigor, note=note, info=info)

def lomax_record(r, mask, held, estimates, shift=0.0):
    """A record in the form replay_lomax reads (hex strings for estimates); logLik moved by `shift`."""
    res = dict(r['result'])
    res['estimates'] = {k: hexed(v) for k, v in estimates.items()}
    if 'logLik' in res: res['logLik'] = hexed(number(res['logLik'])+shift)
    return dict(r, mask=mask, held=held, result=res)

# ------------------------------------------------------------ pareto2

CLAIMS['pareto2'] = {
    'free': ('lomax certificate on y = x - min, shape = kappa, scale = 1/lambda: root half width 8e-7 in z, gain 1e-8, global cover',
             'lomax/certificate.hpp:100-101,118-119 (enclosure), :8 (margin); pareto2/mle.hpp:94-95 delegates with y = x - min'),
    'shape held': ('held-kappa root on log(lambda), half width 0.75e-10', 'lomax/mle.hpp:273-288,299-305 (held-kappa bracket); pareto2/mle.hpp:94-95'),
    'zeros, both free': ('unbounded (exact predicate); the shape-held root with zeros is not among the stored cases', 'pareto2/mle.hpp:23-27,50'),
}

def pareto2_data(data, minimum):
    return [float(v)-minimum for v in data]

def replay_pareto2(r, data, budget):
    mask = r['mask']; res = r['result']; kind = (res.get('outcome') or {}).get('kind')
    if mask & 4 == 0 or mask == 7: return [], 'min free or all held: no certificate comparison'
    minimum = r['held']['min']
    y = pareto2_data(data, minimum)
    if any(v == 0 for v in y): return replay_pareto2_zeros(r, data, y, budget)
    if kind not in ('estimate', 'limit'): return [], None
    out, why = None, None
    held = {}
    if mask == 4:
        lm = 0
        est = {} if kind == 'limit' else dict(kappa=number(res['estimates']['shape']), lambda_=1/number(res['estimates']['scale']))
    elif mask == 5:
        lm = 2; held = {'kappa': r['held']['shape']}
        est = dict(kappa=r['held']['shape'], lambda_=1/number(res['estimates']['scale']))
    else:   # mask 6: scale held, closed form in the shape
        return [], 'closed form (scale held)'
    est = {('lambda' if k == 'lambda_' else k): v for k, v in est.items()}
    rec = lomax_record(r, lm, held, est)
    comps, why = LOMAX(rec, y, budget)
    if lm == 0 and kind == 'estimate': comps.append(lomax_shape(est['kappa'], est['lambda'], y))
    for c in comps:
        c['note'] = ('pareto2 as lomax on x - min; ' + (c['note'] or '')).strip()
        if kind == 'limit' and c['name'] == 'face':
            c['note'] += '; outcome limit(certified = %s), lomax maps the same comparison to limit(false)' % res['outcome'].get('certified')
    if kind == 'limit' and comps:
        info = comps[0].get('info')
        comps.append(comparison('mapping', 'pareto2 certifies the exponential limit (certified = %s)' % res['outcome'].get('certified'),
                                           'info', 'face comparison %s, supremum class %s' % (comps[0]['verdict'], info), rigor='interval',
                                           note='lomax returns limit(false) for this comparison; pareto2 limit(true)'))
    return comps, why

# ------------------------------------------------------------ invpareto

CLAIMS['invpareto'] = {
    'free': ('lomax certificate on y = 1/x, shape = kappa, scale = lambda', 'lomax/certificate.hpp:100-101,118-119; invpareto/mle.hpp:32-33'),
    'shape held': ('held-kappa root on log(lambda), half width 0.75e-10', 'lomax/mle.hpp:273-288,299-305 (held-kappa bracket); invpareto/mle.hpp:42'),
}

def replay_invpareto(r, data, budget):
    mask = r['mask']; res = r['result']; kind = (res.get('outcome') or {}).get('kind')
    if kind not in ('estimate', 'limit'): return [], None
    if mask == 3: return [], 'evaluation only'
    y = [1.0/float(v) for v in data]
    if mask == 2: return [], 'closed form (scale held)'
    if mask == 0:
        est = {} if kind == 'limit' else dict(kappa=number(res['estimates']['shape']), lambda_=number(res['estimates']['scale']))
        lm, held = 0, {}
    else:
        lm, held = 2, {'kappa': r['held']['shape']}
        est = dict(kappa=r['held']['shape'], lambda_=number(res['estimates']['scale']))
    est = {('lambda' if k == 'lambda_' else k): v for k, v in est.items()}
    jac = -2*sum(math.log(float(v)) for v in data)
    comps, why = LOMAX(lomax_record(r, lm, held, est, -jac), y, budget)
    if lm == 0 and kind == 'estimate': comps.append(lomax_shape(est['kappa'], est['lambda'], y))
    for c in comps:
        c['note'] = ('invpareto as lomax on 1/x (logLik = lomax + %.6g); ' % jac + (c['note'] or '')).strip()
    return comps, why

def lomax_shape(estimates_kappa, lam, y):
    """kappa_hat A(z) = 1 on the root enclosure z_hat -/+ 8e-7 widened by the certificate's 1e-8 (lomax/certificate.hpp:118-119): the shape
    coefficient the transfers return is kappa = 1/A(z) of the Lomax fit on y."""
    p = LOMAX_PROFILE(y)
    zc = mp.mpf((iv.log(iv.mpf(lam)*p.xbar)).a)/2+mp.mpf((iv.log(iv.mpf(lam)*p.xbar)).b)/2
    W = mp.mpf('8e-7')
    _, Al, _, _ = p.at(zc-W); _, Ar, _, _ = p.at(zc+W)
    lk = iv.log(iv.mpf(estimates_kappa))
    lo = -iv.log(Ar)-iv.mpf('1e-8'); hi = -iv.log(Al)+iv.mpf('1e-8')
    inside_lo, inside_hi = lk-lo, hi-lk
    verdict = 'holds' if lo_(inside_lo) > 0 and lo_(inside_hi) > 0 else 'fails' if hi_(inside_lo) < 0 or hi_(inside_hi) < 0 else 'open'
    return comparison('coefficient', 'log(kappa_hat) + log A(z) within 1e-8 over the root enclosure z_hat -/+ 8e-7', verdict,
                      'log kappa_hat %s in [%s, %s]' % (show(lk), show(lo), show(hi)), margin='1e-8', note='kappa = 1/A(z), lomax/certificate.hpp:118-119')

def replay_pareto2_zeros(r, data, y, budget):
    """A value equal to min: pareto2's own branch. Only the exact predicate for nonexistence is replayed;
    the shape-held score root with zeros (pareto2/mle.hpp:90-103) does not occur among the stored cases."""
    mask = r['mask']; outcome = r['result'].get('outcome') or {}; kind = outcome.get('kind')
    zeros = sum(1 for v in y if v == 0); n = len(y)
    free_shape, free_scale = not mask & 1, not mask & 2
    if kind not in ('estimate', 'limit', 'nonexistence'): return [], 'zeros, outcome %s: not replayed' % kind
    truth = (zeros == n and (free_shape or free_scale)) or (free_shape and free_scale)
    claimed = kind == 'nonexistence' and outcome.get('reason') == 'unbounded'
    if not truth and not claimed: return [], 'zeros with a bounded likelihood: pareto2/mle.hpp:50-103 is not replayed'
    return [comparison('support', 'unbounded: min attained by %d of %d observations, shape free %s, scale free %s; outcome %s' % (zeros, n, free_shape, free_scale, kind),
                       'holds' if truth == claimed else 'fails', 'predicate (zeros == n and one free) or both free = %s' % truth, rigor='exact',
                       note='dossier predicate, pareto2/mle.hpp:27')], None

# ------------------------------------------------------------ generic one-dimensional cover

def lo_(v): return mp.mpf(v.a)
def hi_(v): return mp.mpf(v.b)

def sign_verdict(value, side):
    """'holds' if the interval lies strictly on `side` (+1 above zero, -1 below)."""
    lo, hi = lo_(value), hi_(value)
    if side > 0: return 'holds' if lo > 0 else 'fails' if hi < 0 else 'open'
    return 'holds' if hi < 0 else 'fails' if lo > 0 else 'open'

def show(v, digits=15):
    if v is None: return None
    if isinstance(v, iv.mpf): return '[%s, %s]' % (mp.nstr(lo_(v), digits), mp.nstr(hi_(v), digits))
    return mp.nstr(v, digits) if not isinstance(v, (str, float)) else str(v)

def cover(value, deriv, upper, lo, hi, target, exclude=(), budget=4000):
    """Is f(t) < target for every t in [lo, hi] outside the open windows in `exclude`?

    value(t): interval enclosure at an exact point; deriv(a, b): enclosure of f' on [a, b];
    upper(a, b): an upper bound of f on [a, b] or None. A cell [a, b] is closed when
    upper(a, b) < target, when f' has one sign on it and the end that carries the maximum is below
    target, or when the two tangent lines from the ends (slopes max(f') above, min(f') below)
    meet below target. A point with f > target is a counterexample ('fails').
    Returns (verdict, info) with info the evaluation count and the largest value seen."""
    parts = [(mp.mpf(lo), mp.mpf(hi))]
    for wl, wr in sorted(exclude):
        nxt = []
        for a, b in parts:
            if wr <= a or wl >= b: nxt.append((a, b)); continue
            if a < wl: nxt.append((a, wl))
            if wr < b: nxt.append((wr, b))
        parts = nxt
    cache = {}
    evals = 0
    best = [mp.mpf('-inf'), None]
    def f(t):
        nonlocal evals
        if t not in cache:
            cache[t] = value(t); evals += 1
            if lo_(cache[t]) > best[0]: best[0], best[1] = lo_(cache[t]), t
        return cache[t]
    heap = []; counter = [0]
    def push(a, b):
        fa, fb = f(a), f(b)
        if lo_(fa) > target: return ('counter', a, fa)
        if lo_(fb) > target: return ('counter', b, fb)
        u = upper(a, b) if upper else None
        if u is not None and hi_(u) < target: return None
        D = deriv(a, b)
        if hi_(D) < 0 and hi_(fa) < target: return None
        if lo_(D) > 0 and hi_(fb) < target: return None
        # tangent lines: f <= fa + dhi (t-a) and f <= fb + dlo (t-b) (dlo <= f')
        dhi, dlo = max(hi_(D), mp.mpf(0)), min(lo_(D), mp.mpf(0))
        if not (mp.isfinite(dhi) and mp.isfinite(dlo)):
            counter[0] += 1
            heapq.heappush(heap, (mp.ninf, counter[0], a, b))
        elif dhi > dlo:
            ya, yb = hi_(fa), hi_(fb)
            # lines meet at t - a = (yb - ya + dlo*(a-b)*-1 ...): solve ya + dhi s = yb + dlo (s - (b-a))
            s = (yb-ya-dlo*(b-a))/(dhi-dlo)
            if 0 <= s <= b-a: bound = ya+dhi*s
            else: bound = max(ya, yb)
            if bound < target: return None
            counter[0] += 1
            heapq.heappush(heap, (-bound, counter[0], a, b))
        else:
            counter[0] += 1
            heapq.heappush(heap, (-max(hi_(fa), hi_(fb)), counter[0], a, b))
        return None
    for a, b in parts:
        if a < b:
            hit = push(a, b)
            if hit: return 'fails', dict(point=hit[1], value=hit[2], evaluations=evals, best=best)
    while heap:
        if evals > budget:
            return 'open', dict(reason='budget of %d evaluations' % budget, upper=-heap[0][0], best=best, evaluations=evals)
        _, _, a, b = heapq.heappop(heap)
        m = (a+b)/2
        if not a < m < b: return 'open', dict(reason='interval at working precision', best=best, evaluations=evals)
        for sub in ((a, m), (m, b)):
            hit = push(*sub)
            if hit: return 'fails', dict(point=hit[1], value=hit[2], evaluations=evals, best=best)
    return 'holds', dict(evaluations=evals, best=best)

def cover_comparison(name, claim, verdict, info, target, rigor='interval', note='', extra=''):
    best, where = info['best']
    hp = 'target %s; sup outside the window below it; largest value seen %s at %s%s' % (
        mp.nstr(target, 15), mp.nstr(best, 15), mp.nstr(where, 12) if where is not None else '-', extra)
    if verdict == 'fails':
        hp = 'f(%s) = %s > target %s' % (mp.nstr(info['point'], 12), show(info['value']), mp.nstr(target, 15))
    if verdict == 'open': hp = '%s; open: %s' % (hp, info.get('reason'))
    return comparison(name, claim, verdict, hp, rigor=rigor, note='%d evaluations%s' % (info['evaluations'], '; '+note if note else ''))

def windowed(function, a, b):
    """Interval at an interval argument."""
    return function(iv.mpf([a, b]))

# ------------------------------------------------------------ cauchy, scale held (F)

CLAIMS['cauchy'] = {
    'scale held': ('the winning root bracket has width <= 1e-10 (scale units), signed score ends and strict concavity on the box padded by '
                   '1e-7 max(1, |end|); the returned point lies within 1e-10 of the bracket; every other candidate is below the winner by a margin; '
                   'global cover of L(t) = -sum log(1 + (y_i - t)^2) on [min y, max y], monotone outside',
                   'cauchy/mle.hpp:306-308 (score and likelihood margins), :326 (concavity pad 1e-7), :345 (bracket to 1e-10), :380-386 (other candidates), :399 (returned point within 1e-10 of the bracket); README "Held-scale location certificate"'),
}

class CauchyLocation:
    """L(t) = -sum log(1 + (y_i - t)^2), y_i = x_i / scale, t = location / scale; termwise bounds as in the dossier."""
    def __init__(self, x, scale):
        s = iv.mpf(scale)
        self.y = [iv.mpf(v)/s for v in x]
        self.n = len(x)
    def value(self, t):
        t = iv.mpf(t); total = iv.mpf(0)
        for y in self.y: total -= iv.log(1+(y-t)**2)
        return total
    @staticmethod
    def q(z): return 2*z/(1+z*z)
    @staticmethod
    def h(z): return 2*(z*z-1)/(1+z*z)**2
    def span(self, y, a, b):
        l, h = y-iv.mpf(b), y-iv.mpf(a)       # residuals z = y - t lie in [l, h]
        return l, h
    def deriv(self, a, b):
        lows, highs = [], []
        one, root3 = mp.mpf(1), mp.sqrt(3)
        for y in self.y:
            l, h = self.span(y, a, b)
            ql, qh = self.q(l), self.q(h)
            lo = min(lo_(ql), lo_(qh)); hi = max(hi_(ql), hi_(qh))
            if lo_(l) <= -one and hi_(h) >= -one: lo = mp.mpf(-1)
            if lo_(l) <= one and hi_(h) >= one: hi = mp.mpf(1)
            lows.append(lo); highs.append(hi)
        return iv.mpf([mp.fsum(lows), mp.fsum(highs)])
    def curvature(self, a, b):
        total = iv.mpf(0); root3 = mp.sqrt(3)
        for y in self.y:
            l, h = self.span(y, a, b)
            top = max(hi_(self.h(l)), hi_(self.h(h)))
            if (lo_(l) <= -root3 and hi_(h) >= -root3) or (lo_(l) <= root3 and hi_(h) >= root3): top = max(top, mp.mpf(1)/4)
            total += iv.mpf(top)
        return total
    def upper(self, a, b):
        total = iv.mpf(0)
        for y in self.y:
            l, h = self.span(y, a, b)
            d = lo_(l) if lo_(l) > 0 else (-hi_(h) if hi_(h) < 0 else mp.mpf(0))
            total -= iv.log(1+iv.mpf(d)**2)
        return total

def replay_cauchy_scale(r, data, budget):
    res = r['result']; kind = (res.get('outcome') or {}).get('kind')
    if r['mask'] != 2 or kind != 'estimate': return [], None
    x = [float(v) for v in data]; scale = r['held']['scale']
    loc = number(res['estimates']['location'])
    p = CauchyLocation(x, scale)
    that = mp.mpf(loc)/mp.mpf(scale)
    ymax = max(abs(mp.mpf(v)/mp.mpf(scale)) for v in x)
    w = mp.mpf('2e-10')+8*mp.mpf(EPS)*(1+ymax+abs(that))
    delta = mp.mpf('1e-7')*max(1, abs(that))+w
    out = []
    # bracket: the score changes sign across the returned point within w
    gl, gr = p.deriv(that-w, that-w), p.deriv(that+w, that+w)
    vl, vr = sign_verdict(gl, +1), sign_verdict(gr, -1)
    bracket = 'fails' if 'fails' in (vl, vr) else 'holds' if vl == vr == 'holds' else 'open'
    out.append(comparison('bracket', "L'(t_hat - w) > 0 > L'(t_hat + w), w = 2e-10 + rounding", bracket,
                          'left %s, right %s' % (show(gl), show(gr)), margin='128 eps n = %.3g' % (128*EPS*len(x)),
                          note='w = %s' % mp.nstr(w, 4)))
    cu = p.curvature(that-delta, that+delta)
    curv = sign_verdict(cu, -1)
    out.append(comparison('curvature', "L'' < 0 on t_hat +- (1e-7 max(1,|t_hat|) + w) (termwise bound)", curv, show(cu), margin='128 eps n',
                          note='the claimed concavity of the padded box'))
    unique = (res.get('outcome') or {}).get('unique')
    ft = p.value(that); target = lo_(ft)
    lo = lo_(min(p.y, key=lambda v: lo_(v))) ; hi = hi_(max(p.y, key=lambda v: hi_(v)))
    if bracket != 'holds' or curv != 'holds':
        out.append(comparison('cover', 'global maximum within the window', 'open', 'not run: the window is not established'))
        return out, None
    excl = [(that-delta, that+delta)]
    if unique is False:   # two-point tie: the mirror window carries the same value
        if len(x) != 2: out.append(comparison('cover', 'non-unique maximum', 'open', 'only n = 2 symmetric ties are replayed')); return out, None
        mirror = lo_(p.y[0])/2+hi_(p.y[0])/2+lo_(p.y[1])/2+hi_(p.y[1])/2-that
        excl.append((mirror-delta, mirror+delta))
        tie = p.value(mirror)-ft
        out.append(comparison('tie', 'the mirror point carries the same likelihood (two-point symmetry)',
                              'holds' if lo_(tie) < 1e-30 and hi_(tie) > -1e-30 else 'open', show(tie), note='n = 2'))
    verdict, info = cover(p.value, p.deriv, p.upper, lo, hi, target, excl, budget=budget)
    out.append(cover_comparison('cover', 'L(t) < L(t_hat) for every t in [min y, max y] outside the window (tails are monotone)', verdict, info, target,
                                extra='; window half width %s' % mp.nstr(delta, 4)))
    return out, None


# ------------------------------------------------------------ gpd, joint and held scale (F)

GPD_MARGIN = mp.mpf('1e-8')
CLAIMS['gpd'] = {
    'joint (sigma, xi free)': ('estimate: signed root enclosure narrower than 2e-7 in t, or 3e-7 in the coefficients (restoration within 1e-7); every cell outside the root '
                               'closed with likelihood bound below the best root minus 1e-8; winner above the uniform boundary by 1e-8; negative tail at t = -32 '
                               'or the A = 1 crossing, positive tail by the sign of the score; xi = -1 (uniform boundary) when every interior cell is below 0',
                               'gpd/certificate.hpp:7 (margin), :83-134 (isolate; window :114-116), :156-167 (negative tail), :185-189 (positive tail), :228 (cell exclusion), :261-263 (uniform floor and tie margins), :271-275 (restoration); README "Certificate"'),
    'sigma held': ('same with the held-scale profile h = -(1/c + b) D, c = sigma/M < 1 (sigma >= M returns the uniform member: exact predicate, not replayed); '
                   'negative tail from the score sign (1-c)(1-e^t)w > e^t(-t), positive tail c b > 2t and b m > 1',
                   'gpd/certificate.hpp:176-181 (held-scale negative tail), :185-189 (positive tail); README "Certificate"'),
}

def as_iv(t): return t if isinstance(t, iv.mpf) else iv.mpf(t)

class GpdProfile:
    """D, P, R of the dossier on v_i = (x_i - mu)/M, as enclosures at a point or on an interval t."""
    def __init__(self, y, held_c=None):
        M = max(y)
        counts = collections.Counter(y)
        self.n = len(y); self.M = M
        self.vs = [(iv.mpf(v)/iv.mpf(M), c, v) for v, c in sorted(counts.items())]
        self.min_positive = min(v for v in y if v > 0)/M
        self.c = held_c            # None: joint profile; else the held scale in units of M (iv)
        self.cache = {}
    def DPR(self, t):
        tt = as_iv(t)
        key = mp.mpf(tt.a) if tt.a == tt.b else None
        if key is not None and key in self.cache: return self.cache[key]
        b = iv.expm1(tt); inf = mp.inf
        bzero = lo_(b) <= 0 <= hi_(b)
        D = P = R = iv.mpf(0)
        for v, count, raw in self.vs:
            z = b*v
            zmax = max(abs(lo_(z)), abs(hi_(z)))
            if zmax < mp.mpf('0.9') and (zmax < mp.mpf('0.25') or bzero):
                K = int(40/max(-mp.log10(zmax), mp.mpf('0.04')))+3 if zmax > 0 else 3
                a = p = r = iv.mpf(0); power = iv.mpf(1)
                for k in range(K):
                    a += power/(k+1); p += power*(k+1)/(k+2); r += power*(k+1)*(k+2)/(k+3); power *= -z
                tail = iv.mpf([-1, 1])*(zmax**K/(1-zmax))*(K+3)
                a = (a+tail)*v; p = (p+tail)*v*v; r = (r+tail)*v*v*v
            elif bzero:
                a = p = r = iv.mpf([-inf, inf])
            else:
                den = 1+z
                l = iv.log(den); q = z/den
                a = l/b; p = (l-q)/(b*b); r = (2*l-2*q-q*q)/(b*b*b)
            D += count*a; P += count*p; R += count*r
        out = (D/self.n, P/self.n, R/self.n)
        if key is not None: self.cache[key] = out
        return out
    def h(self, t):
        D, P, R = self.DPR(t); b = iv.expm1(as_iv(t))
        if self.c is None: return -iv.log(D)-b*D-1
        return -(1/self.c+b)*D
    def score(self, t):
        D, P, R = self.DPR(t); tt = as_iv(t)
        b = iv.expm1(tt); e = iv.exp(tt)
        if self.c is None: return e*(P/D-D+b*P)
        return e*(-D+(1/self.c+b)*P)
    def curvature(self, t):
        D, P, R = self.DPR(t); tt = as_iv(t)
        b = iv.expm1(tt); e = iv.exp(tt)
        g = (P/D-D+b*P) if self.c is None else (-D+(1/self.c+b)*P)
        k = (P*P/(D*D)-R/D+2*P-b*R) if self.c is None else (2*P-(1/self.c+b)*R)
        return e*e*k+e*g
    def A(self, t):
        D, P, R = self.DPR(t); return -iv.expm1(as_iv(t))*D
    def shape(self, t):
        D, P, R = self.DPR(t); b = iv.expm1(as_iv(t))
        return b*D if self.c is None else self.c*b

def gpd_t_of_shape(p, xi):
    """t at which the profile's shape coefficient equals xi (numerical root of a monotone function)."""
    if p.c is not None: return mp.log1p(mp.mpf(xi)/mp.mpf(p.c.a))
    def g(t):
        s = p.shape(mp.mpf(t)); return (mp.mpf(s.a)+mp.mpf(s.b))/2-xi
    return mp.findroot(g, mp.mpf(0), solver='secant', tol=mp.mpf(10)**-30, maxsteps=100, verify=False)

def replay_gpd(r, data, budget):
    res = r['result']; kind = (res.get('outcome') or {}).get('kind'); mask = r['mask']
    if kind != 'estimate' or mask not in (1, 3): return [], None
    mu = r['held']['mu']
    y = [float(v)-mu for v in data]
    M = max(y)
    if not M > 0: return [], 'no positive excess'
    xi = number(res['estimates']['xi']); sigma = number(res['estimates']['sigma'])
    held = mask == 3
    if held and r['held']['sigma'] >= M: return [], 'sigma held at or above the maximum excess: uniform member (exact predicate)'
    if min(v for v in y if v > 0)/M < 2.2250738585072014e-308: return [], 'subnormal normalized excess: certificate stops'
    c = (iv.mpf(r['held']['sigma'])/iv.mpf(M)) if held else None
    p = GpdProfile(y, c)
    out = []
    n = len(y)
    uniform = (xi == -1.0 and not held)
    exclude = []
    if uniform:
        that = None; target = mp.mpf(0)
    else:
        bhat = iv.mpf(xi)*iv.mpf(M)/iv.mpf(sigma)
        that = mp.log1p(mp.mpf(bhat.a)/2+mp.mpf(bhat.b)/2)
        wt = mp.mpf('3e-7'); m = max(1, abs(mp.mpf(xi)))
        try: ta, tb = gpd_t_of_shape(p, mp.mpf(xi)-wt*m), gpd_t_of_shape(p, mp.mpf(xi)+wt*m)
        except (ValueError, ZeroDivisionError): ta, tb = that-wt, that+wt
        lo_w, hi_w = min(that-wt, ta, tb), max(that+wt, ta, tb)
        sl, sr = p.score(lo_w), p.score(hi_w)
        vl, vr = sign_verdict(sl, +1), sign_verdict(sr, -1)
        root = 'fails' if 'fails' in (vl, vr) else 'holds' if vl == vr == 'holds' else 'open'
        out.append(comparison('bracket', 'h_t(t_lo) > 0 > h_t(t_hi) on the window of the claimed enclosure (3e-7 in t or in the coefficients)', root,
                              'window [%s, %s] around t_hat = %s; left %s, right %s' % (mp.nstr(lo_w, 10), mp.nstr(hi_w, 10), mp.nstr(that, 12), show(sl), show(sr)),
                              margin='32 eta (1 + |t|), eta = %.3g' % (512*EPS*(1+math.log(n+1))), note='window from certificate.hpp:83-134'))
        half = 5*max(that-lo_w, hi_w-that)
        W2 = (that-half, that+half)
        cu = p.curvature(iv.mpf(list(W2)))
        curv = sign_verdict(cu, -1)
        out.append(comparison('curvature', 'h_tt < 0 on the five-fold window (unique stationary point)', curv, show(cu),
                              margin='32 eta (1 + |h_tt|)', note='interval evaluation on the window'))
        ft = p.h(that); target = lo_(ft)
        if root != 'holds' or curv != 'holds':
            out.append(comparison('cover', 'global maximum in the window', 'open', 'not run: window not established'))
            return out, None
        exclude = [W2]
        if not held:
            out.append(comparison('gain', 'h(t_hat) > 1e-8: the interior optimum beats the uniform boundary', sign_verdict(ft-iv.mpf(GPD_MARGIN), +1),
                                  show(ft), margin=1e-8))
    hi = mp.mpf(1)
    def tail_ok(t):
        b = mp.expm1(t)
        if held: return mp.mpf(c.a)*b > 2*t and b*mp.mpf(p.min_positive) > 1
        return b*mp.mpf(p.min_positive) > t
    while hi < 64 and not tail_ok(hi): hi *= 2
    if not tail_ok(hi): return out+[comparison('cover', 'positive tail', 'open', 'tail inequality not reached by t = 64')], None
    tail_note = 'right tail: score < 0 beyond t = %s' % mp.nstr(hi, 6)
    if not held:
        lo = mp.mpf(-32)
        A0 = p.A(lo)
        if lo_(A0) >= 1:
            l, h_ = mp.mpf(-32), mp.mpf(0)
            for _ in range(60):
                mid = (l+h_)/2
                if lo_(p.A(mid)) > 1: l = mid
                else: h_ = mid
            lo = l
            tail_upper = mp.mpf(0); tail_note += '; left: A > 1 below t = %s (uniform dominates, bound 0)' % mp.nstr(lo, 8)
        else:
            tail_upper = hi_(-iv.log(A0)+A0-1); tail_note += '; left tail bound -log A + A - 1 = %s at t = -32' % mp.nstr(tail_upper, 6)
        tail_verdict = 'holds' if (tail_upper < target if that is not None else tail_upper == 0) else 'fails'
    else:
        lo = mp.mpf(-1)
        w = mp.mpf(sum(count for v, count, raw in p.vs if raw == M))/n
        cc = mp.mpf(c.a)
        def left_ok(t): return (1-cc)*(-mp.expm1(t))*w > mp.exp(t)*(-t)
        while lo > -32 and not left_ok(lo): lo *= 2
        if not left_ok(lo): return out+[comparison('cover', 'negative tail', 'open', 'held-scale tail inequality not reached by t = -32')], None
        tail_note += '; left: score > 0 below t = %s' % mp.nstr(lo, 6)
        tail_verdict = 'holds'
    verdict, info = cover(p.h, lambda a, b: p.score(iv.mpf([a, b])), None, lo, hi, target, exclude, budget=budget*8)
    if verdict == 'holds' and tail_verdict != 'holds': verdict = tail_verdict
    out.append(cover_comparison('cover', 'h(t) < %s for every t in [%s, %s] outside the window, and both analytic tails' % (
                                'h(t_hat)' if that is not None else '0 (uniform boundary)', mp.nstr(lo, 6), mp.nstr(hi, 6)), verdict, info, target, note=tail_note))
    return out, None


def replay_gpd_coefficient(r, data, budget):
    """The returned scale of a joint fit matches the profile's D at the root window: |sigma_hat / M - D(t*)| <= 1e-8 D (certificate.hpp:274)."""
    res = r['result']; kind = (res.get('outcome') or {}).get('kind')
    if kind != 'estimate' or r['mask'] != 1: return [], None
    y = [float(v)-r['held']['mu'] for v in data]; M = max(y)
    xi = number(res['estimates']['xi']); sigma = number(res['estimates']['sigma'])
    if not M > 0 or xi == -1.0: return [], None
    p = GpdProfile(y, None)
    bhat = iv.mpf(xi)*iv.mpf(M)/iv.mpf(sigma); that = mp.log1p(mp.mpf(bhat.a)/2+mp.mpf(bhat.b)/2)
    wt = mp.mpf('3e-7'); m = max(1, abs(mp.mpf(xi)))
    try: ta, tb = gpd_t_of_shape(p, mp.mpf(xi)-wt*m), gpd_t_of_shape(p, mp.mpf(xi)+wt*m)
    except (ValueError, ZeroDivisionError): ta, tb = that-wt, that+wt
    lo_w, hi_w = min(that-wt, ta, tb), max(that+wt, ta, tb)
    D_hi, D_lo = p.DPR(hi_w)[0], p.DPR(lo_w)[0]          # D decreases in t
    s = iv.mpf(sigma)/iv.mpf(M)
    low = D_hi*(1-iv.mpf('1e-8')); high = D_lo*(1+iv.mpf('1e-8'))
    d1, d2 = s-low, high-s
    verdict = 'holds' if lo_(d1) > 0 and lo_(d2) > 0 else 'fails' if hi_(d1) < 0 or hi_(d2) < 0 else 'open'
    return [comparison('coefficient', 'sigma_hat / M within 1e-8 (relative) of D(t) for the root window', verdict,
                       'sigma_hat/M = %s in [%s, %s]' % (show(s), show(low), show(high)), margin='1e-8', note='certificate.hpp:274')], None

# ------------------------------------------------------------ foldnorm, joint (F)

CLAIMS['foldnorm'] = {
    'joint (mu, sigma free)': ('the grid incumbent is certified by convex-chord bounds: the profile upper bound over every cell is <= best + tol n, tol = 1e-10; '
                               'the interior root is bracketed with relative width 0.25 tol in mu and sigma (the location with absolute scale max(mu, sigma)); '
                               'mu = 0 (half-normal) when no interior candidate exceeds it by 1e-11 n',
                               'foldnorm/mle.hpp:152,193 (upper <= best + tol n), :206 (boundary preference 1e-11 n), :236-246 (bracket), :294-295 (returned coefficients)'),
}

class FoldProfile:
    """Profile in a = mu/sigma^2 of the folded normal: sigma^2 solves s + a^2 s^2 = mean(x^2) (foldnorm README, R notes), so
    l(a)/n = -log(s)/2 - m2/(2 s) - a^2 s/2 + mean(log cosh(a x)) + const and l'(a)/n = -a s + mean(x tanh(a x)) by the envelope theorem."""
    def __init__(self, x, unit=1.0):
        counts = collections.Counter(x)
        self.n = len(x)
        self.pts = [(iv.mpf(v)/iv.mpf(unit), iv.mpf(c)/self.n) for v, c in sorted(counts.items())]   # exact scaling by the unit
        self.xbar = sum((w*v for v, w in self.pts), iv.mpf(0))
        self.m2 = sum((w*v*v for v, w in self.pts), iv.mpf(0))
    def sigma2(self, a):
        return 2*self.m2/(1+iv.sqrt(1+4*a*a*self.m2))
    def value(self, a):          # per observation, up to the constant log(2/sqrt(2 pi))
        a = as_iv(a); u = self.sigma2(a)
        total = -iv.log(u)/2-self.m2/(2*u)-a*a*u/2
        for v, w in self.pts:
            y = a*v
            total += w*(y+iv.log1p(iv.exp(-2*y))-iv.log(iv.mpf(2)))
        return total
    def deriv(self, a):
        a = as_iv(a); u = self.sigma2(a)
        total = -a*u
        for v, w in self.pts:
            e = iv.exp(-2*a*v)
            total += w*v*(1-e)/(1+e)
        return total
    def deriv_range(self, a, b):
        """Enclosure of l' on [a, b]: both a sigma^2 = 2 a m2/(1 + sqrt(1 + 4 a^2 m2)) and mean(x tanh(a x)) increase in a, so the
        range is [T(a) - g(b), T(b) - g(a)] (no dependency loss)."""
        def parts(a):
            a = iv.mpf(a); g = 2*a*self.m2/(1+iv.sqrt(1+4*a*a*self.m2)); T = iv.mpf(0)
            for v, w in self.pts:
                e = iv.exp(-2*a*v); T += w*v*(1-e)/(1+e)
            return g, T
        ga, Ta = parts(a); gb, Tb = parts(b)
        return iv.mpf([min(lo_(Ta-gb), lo_(Tb-ga)), max(hi_(Ta-gb), hi_(Tb-ga))])

def replay_foldnorm(r, data, budget):
    res = r['result']; kind = (res.get('outcome') or {}).get('kind')
    if kind != 'estimate' or r['mask'] != 0: return [], None
    x = [float(v) for v in data]; n = len(x)
    if len(set(x)) == 1: return [], 'constant sample'
    mu = number(res['estimates']['mu']); sigma = number(res['estimates']['sigma'])
    if not (sigma > 0 and mu >= 0 and math.isfinite(sigma) and math.isfinite(mu)): return [], 'returned coefficients outside the model'
    xs = max(x)
    p = FoldProfile(x, xs)      # scale covariance: x -> x/xs, a -> a xs, the profile moves by a constant (exact scaling)
    var = p.m2-p.xbar*p.xbar
    if not lo_(var) > 0: return [], 'variance enclosure contains 0'
    ahat = mp.mpf(mu)/mp.mpf(sigma)**2*mp.mpf(xs)
    out = []
    tol = mp.mpf('1e-10')
    ft = p.value(ahat); target = lo_(ft)+tol    # per observation: tolerance tol (per-observation scale of the n-scaled bound)
    # note: the code compares total bounds with best + tol n, i.e. the same tolerance per observation
    if mu > 0:
        w = mp.mpf('1e-10')*(max(mp.mpf(mu), mp.mpf(sigma))/mp.mpf(sigma)**2*mp.mpf(xs)+2*ahat)
        gl, gr = p.deriv(ahat-w), p.deriv(ahat+w)
        vl, vr = sign_verdict(gl, +1), sign_verdict(gr, -1)
        bracket = 'fails' if 'fails' in (vl, vr) else 'holds' if vl == vr == 'holds' else 'open'
        out.append(comparison('bracket', "l'(a_hat - w) > 0 > l'(a_hat + w), w = 1e-10 (max(mu, sigma)/sigma^2 + 2 a_hat)", bracket,
                              'left %s, right %s' % (show(gl), show(gr)), margin='32 eps', note='w = %s; a = mu/sigma^2' % mp.nstr(w, 4)))
    hi = mp.mpf((p.xbar/var).b)*(1+mp.mpf('1e-9'))
    verdict, info = cover(p.value, p.deriv_range, None, mp.mpf(0), hi, target, [], budget=budget*20)
    out.append(cover_comparison('cover', 'l(a) < l(a_hat) + 1e-10 per observation for every a in [0, xbar/(mean(x^2)-xbar^2)] (decreasing beyond: l\' <= n(xbar - a s) < 0)',
                                verdict, info, target, note='target is the profile value at a_hat plus tol; a_hat = mu/sigma^2 from the returned coefficients'))
    return out, None

# ------------------------------------------------------------ fatigue, free scale and held alpha (F)

CLAIMS['fatigue'] = {
    'shift held, alpha and beta free': ('every root of the profile score on [H, A] (harmonic and arithmetic mean of x - mu; the score points inward outside) is isolated by rational '
                                        'interval bounds; the winning root is bracketed to relative width 0.01 tol = 1e-12 (alpha endpoint width likewise); another candidate '
                                        'more than 1e-9 away in log beta within 1e-10 n of the winner stops',
                                        'fatigue/certificate.hpp:40 (domain [H, A]), :62-72 (bounds), :88-90 (bisection to .01 tol), :111 (tie 1e-10 n, 1e-9); README "Details"'),
    'shift held, alpha held': ('same on [t_min / r, t_max r], r = (alpha/2 + sqrt(alpha^2/4 - 1))^2 for alpha > 2, else r = 1 (exterior roots of the individual scores)',
                               'fatigue/certificate.hpp:41 (domain [t_min/r, t_max r]); README "Details"'),
}

class FatigueProfile:
    """Profile (alpha free) or conditional (alpha held) log likelihood in b = beta of the Birnbaum-Saunders family, up to constants in t,
    on shifted observations t_i = x_i - mu (fatigue README log_density)."""
    def __init__(self, t, alpha=None):
        counts = collections.Counter(t)
        self.n = len(t)
        self.pts = [(iv.mpf(v), iv.mpf(c)) for v, c in sorted(counts.items())]
        self.alpha = None if alpha is None else iv.mpf(alpha)
        self.A = sum((c*v for v, c in self.pts), iv.mpf(0))/self.n
        self.H = self.n/sum((c/v for v, c in self.pts), iv.mpf(0))
    def sums(self, b):
        s_log = sum((c*iv.log(v+b) for v, c in self.pts), iv.mpf(0))
        q = sum((c*(v/b+b/v) for v, c in self.pts), iv.mpf(0))           # sum (t/b + b/t)
        return s_log, q
    def value(self, b):
        b = as_iv(b); s_log, q = self.sums(b); n = self.n
        if self.alpha is None:
            a2 = q/n-2
            return s_log-n*iv.log(b)/2-n*iv.log(a2)/2
        return s_log-n*iv.log(b)/2-q/(2*self.alpha**2)
    def deriv(self, b):
        b = as_iv(b); n = self.n
        s1 = sum((c/(v+b) for v, c in self.pts), iv.mpf(0))
        d = sum((c*(1/v-v/(b*b)) for v, c in self.pts), iv.mpf(0))          # d/db of sum (t/b + b/t)
        if self.alpha is None:
            a2 = sum((c*(v/b+b/v) for v, c in self.pts), iv.mpf(0))/n-2
            return s1-n/(2*b)-d/(2*a2)
        return s1-n/(2*b)-d/(2*self.alpha**2)

def standard_cover(p, that, wb, lo, hi, budget, wexc, what, bracket_claim, bracket_margin):
    """bracket: p.deriv changes sign across [that - wb, that + wb]; cover: p.value < p.value(that) outside [that - wexc, that + wexc] on [lo, hi]."""
    out = []
    gl, gr = p.deriv(that-wb), p.deriv(that+wb)
    vl, vr = sign_verdict(gl, +1), sign_verdict(gr, -1)
    bracket = 'fails' if 'fails' in (vl, vr) else 'holds' if vl == vr == 'holds' else 'open'
    out.append(comparison('bracket', bracket_claim, bracket, 'left %s, right %s' % (show(gl), show(gr)), margin=bracket_margin, note='half width %s' % mp.nstr(wb, 4)))
    ft = p.value(that); base = lo_(ft)
    if bracket != 'holds':
        out.append(comparison('cover', what, 'open', 'not run: bracket not established')); return out
    verdict, info = cover(p.value, lambda a, b: p.deriv(iv.mpf([a, b])), None, lo, hi, base, [(that-wexc, that+wexc)], budget=budget)
    comp = cover_comparison('cover', what, verdict, info, base, note='exclusion window half width %s' % mp.nstr(wexc, 4))
    if verdict == 'fails':   # how far above is the competing maximum? (numerical local refinement, information only)
        try:
            d = lambda b: (lambda z: (mp.mpf(z.a)+mp.mpf(z.b))/2)(p.deriv(mp.mpf(b)))
            f = lambda b: (lambda z: (mp.mpf(z.a)+mp.mpf(z.b))/2)(p.value(mp.mpf(b)))
            root = mp.findroot(d, info['point'], tol=mp.mpf(10)**-35, maxsteps=100)
            comp['note'] += '; numerical local maximum of the competitor: b = %s, l - l(b_hat) = %s (total log-likelihood units; n = %d)' % (
                mp.nstr(root, 12), mp.nstr(f(root)-f(that), 4), p.n)
        except (ValueError, ZeroDivisionError): pass
    out.append(comp)
    return out

def replay_fatigue(r, data, budget):
    res = r['result']; kind = (res.get('outcome') or {}).get('kind'); mask = r['mask']
    if kind != 'estimate' or mask not in (4, 5): return [], None
    mu = r['held']['mu']
    t = [float(v)-mu for v in data]
    if not all(v > 0 for v in t): return [], 'shifted data not positive'
    beta = number(res['estimates']['beta'])
    alpha = r['held']['alpha'] if mask == 5 else None
    p = FatigueProfile(t, alpha)
    if alpha is None:
        if len(set(t)) == 1: return [], 'constant sample'
        lo, hi = mp.mpf(p.H.a), mp.mpf(p.A.b)
        if lo >= hi: return [], 'harmonic and arithmetic mean not separated'
    else:
        rr = mp.mpf(1) if alpha <= 2 else (mp.mpf(alpha)/2+mp.sqrt((mp.mpf(alpha)/2-1)*(mp.mpf(alpha)/2+1)))**2
        lo, hi = mp.mpf(min(t))/rr*(1-mp.mpf('1e-12')), mp.mpf(max(t))*rr*(1+mp.mpf('1e-12'))
        if len(set(t)) == 1: return [], 'constant sample with alpha held: tied reciprocal maxima (stop)'
    that = mp.mpf(beta)
    out = standard_cover(p, that, that*mp.mpf('1.1e-12'), lo, hi, budget*8, that*mp.mpf('1e-9'),
                         'l(b) < l(b_hat) for every b in [%s, %s] outside the log window 1e-9 (score points inward outside)' % (mp.nstr(lo, 6), mp.nstr(hi, 6)),
                         "l'(b_hat (1 - w)) > 0 > l'(b_hat (1 + w)), w = 1.1e-12 (bracket 0.01 tol = 1e-12 plus rounding)", '16 eps per interval operation')
    return out, None

# ------------------------------------------------------------ skellam, both rates free (F, numerical)

CLAIMS['skellam'] = {
    'both rates free': ('the returned likelihood is within 1e-8 n of the global maximum: branch and bound over the profile t = min(mu1, mu2) on the line mu1 - mu2 = xbar with the '
                        'quadratic envelope l(m) + |l\'(m)| h + M h^2/2, M = sum 2/(|k| + 1), a Poisson-mode tail bound for t >= 2, and the positive series bound l(eps) + 2 n eps on [0, eps]; '
                        'faces win ties within 0.5e-8 n',
                        'skellam/README.md "Joint-search certificate" (curvature bound, tails); skellam/mle.hpp margin constants'),
}

def skellam_logpmf(k, m1, m2):
    """log of the Skellam mass at k for rates m1, m2 > 0 (mpmath, ordinary precision: numerical)."""
    v = abs(k); z = 2*mp.sqrt(m1*m2)
    return -(m1+m2)+mp.mpf(k)/2*mp.log(m1/m2)+mp.log(mp.besseli(v, z))

def skellam_loglik(counts, m1, m2):
    """Total log likelihood at (m1, m2) >= 0, faces included (a zero rate: signed Poisson)."""
    total = mp.mpf(0)
    if m1 > 0 and m2 > 0:
        for k, c in counts.items(): total += c*skellam_logpmf(k, m1, m2)
        return total
    if m1 == 0 and m2 == 0: return mp.mpf(0) if all(k == 0 for k in counts) else -mp.inf
    for k, c in counts.items():
        if m2 == 0 and k < 0 or m1 == 0 and k > 0: return -mp.inf
        mu = m1 if m2 == 0 else m2; kk = abs(k)
        total += c*(kk*mp.log(mu)-mu-mp.loggamma(kk+1))
    return total

def skellam_line(counts, n, c1, c2, t):
    """(l(t), l'(t)) on the line m1 = t + c1, m2 = t + c2 (t > 0)."""
    m1, m2 = t+c1, t+c2; z = 2*mp.sqrt(m1*m2); val = mp.mpf(0); der = mp.mpf(0)
    for k, c in counts.items():
        v = abs(k); Iv = mp.besseli(v, z); R = mp.besseli(v+1, z)/Iv+mp.mpf(v)/z
        val += c*(-(m1+m2)+mp.mpf(k)/2*mp.log(m1/m2)+mp.log(Iv))
        der += c*((-1+mp.mpf(k)/(2*m1)+R*mp.sqrt(m2/m1))+(-1-mp.mpf(k)/(2*m2)+R*mp.sqrt(m1/m2)))
    return val, der

def replay_skellam(r, data, budget):
    res = r['result']; kind = (res.get('outcome') or {}).get('kind')
    if kind != 'estimate' or r['mask'] != 0: return [], None
    x = [int(v) for v in data]; n = len(x); counts = collections.Counter(x)
    if len(counts) == 1: return [], 'constant sample (closed form)'
    if max(abs(k) for k in counts) > 5000 or max(number(res['estimates']['mu1']), number(res['estimates']['mu2'])) > 1e5:
        return [comparison('cover', 'global profile maximum', 'open', 'orders above 5000 or rates above 1e5: mpmath Bessel series not run', rigor='numerical')], None
    m1h = number(res['estimates']['mu1']); m2h = number(res['estimates']['mu2'])
    xbar = mp.mpf(sum(x))/n
    c1, c2 = max(xbar, mp.mpf(0)), max(-xbar, mp.mpf(0))
    ll_hat = skellam_loglik(counts, mp.mpf(m1h), mp.mpf(m2h))
    margin = mp.mpf('1e-8')*n
    target = ll_hat+margin
    M = sum(c*mp.mpf(2)/(abs(k)+1) for k, c in counts.items())
    out = []
    # tail: a convolution mass is at most the maximal mass of either Poisson factor, <= 1/sqrt(2 pi floor(t)) for t >= 2
    T = max(mp.mpf(2), mp.floor(mp.exp(-2*target/n)/(2*mp.pi))+2)
    tail_ok = -mp.mpf(n)/2*mp.log(2*mp.pi*mp.floor(T)) < target
    eps = mp.mpf('1e-10')
    try: l_eps, _ = skellam_line(counts, n, c1, c2, eps)
    except (ValueError, ZeroDivisionError, OverflowError, TypeError): l_eps = mp.inf
    face_ok = l_eps+2*n*eps < target
    cells = [(eps, T)]; evals = 0; worst = -mp.inf; where = None; verdict = 'holds'; reason = ''
    while cells:
        if evals > budget*10: verdict, reason = 'open', 'budget'; break
        a, b = cells.pop(); m = (a+b)/2; h = (b-a)/2
        try: lv, ld = skellam_line(counts, n, c1, c2, m)
        except (ValueError, ZeroDivisionError, OverflowError, TypeError) as error: verdict, reason = 'open', 'Bessel evaluation: %s' % type(error).__name__; break
        evals += 1
        if lv > worst: worst, where = lv, m
        if lv > target: verdict = 'fails'; reason = 'l(%s) = %s > target' % (mp.nstr(m, 12), mp.nstr(lv, 15)); break
        if lv+abs(ld)*h+M*h*h/2 < target: continue
        cells.append((a, m)); cells.append((m, b))
    if verdict == 'holds' and not (tail_ok and face_ok): verdict = 'open'; reason = 'tail %s, face bound %s' % (tail_ok, face_ok)
    out.append(comparison('cover', 'sup of the profile on t in [0, T] below l(returned) + 1e-8 n (numerical: mpmath Bessel, no interval version); tail t >= %s' % mp.nstr(T, 6),
                          verdict, 'target %s = l_hat %s + 1e-8 n; largest profile value seen %s at t = %s; %s' % (mp.nstr(target, 15), mp.nstr(ll_hat, 15), mp.nstr(worst, 15),
                                                                                               mp.nstr(where, 10) if where is not None else '-', reason),
                          rigor='numerical', note='%d cells; curvature bound M = %s (dossier)' % (evals, mp.nstr(M, 6))))
    return out, None

# ------------------------------------------------------------ cache and registry

import gzip, hashlib, json, os, pathlib
VERSIONS = {'foldnorm': 4, 'fatigue': 3, 'skellam': 2, 'pareto2': 3, 'invpareto': 3, 'cauchy': 2}      # bump a family's entry when its replay changes; the cache key includes it
VERSION = 'covers-1'
CACHE_PATH = pathlib.Path(os.environ.get('CERT_REPLAY_COVERS_CACHE', '/tmp/cert-replay-covers-cache.jsonl.gz'))
_cache = None

def _load_cache():
    global _cache
    if _cache is None:
        _cache = {}
        if CACHE_PATH.exists():
            try:
                with gzip.open(CACHE_PATH, 'rt') as f:
                    for line in f:
                        k, v = json.loads(line); _cache[k] = v
            except (OSError, EOFError, ValueError): pass
    return _cache

def cached(family, replay):
    """Memoize a replay on (version, family, mask, held values, returned coefficients, outcome, data, budget); a
    result is the comparison list and the reason, both plain JSON. The cache is a gzipped JSON-lines file
    (CERT_REPLAY_COVERS_CACHE, default /tmp); deleting it only costs time. Not shared between dps or budgets."""
    def run(r, data, budget):
        cache = _load_cache()
        key = hashlib.sha256(json.dumps([VERSION, VERSIONS.get(family, 1), family, r['mask'], r['held'], r['result'].get('estimates'), r['result'].get('outcome'),
                                         r['result'].get('logLik'), [float(v).hex() for v in data], budget, mp.mp.dps], sort_keys=True).encode()).hexdigest()
        if key in cache: return cache[key]['comps'], cache[key]['why']
        comps, why = replay(r, data, budget)
        comps = json.loads(json.dumps(comps, default=str))
        cache[key] = dict(comps=comps, why=why)
        with gzip.open(CACHE_PATH, 'at') as f: f.write(json.dumps([key, cache[key]])+'\n')
        return comps, why
    return run

def both(first, second):
    def run(r, data, budget):
        a, why_a = first(r, data, budget); b, why_b = second(r, data, budget)
        return (a or [])+(b or []), why_a or why_b
    return run

REPLAYS = {family: cached(family, fn) for family, fn in
           {'pareto2': replay_pareto2, 'invpareto': replay_invpareto, 'cauchy': replay_cauchy_scale, 'foldnorm': replay_foldnorm,
            'fatigue': replay_fatigue, 'skellam': replay_skellam}.items()}
REPLAYS['gpd'] = both(cached('gpd', replay_gpd), cached('gpd-coefficient', replay_gpd_coefficient))
