"""Certificate replay, mechanisms E and F: Lomax transfers and one-dimensional covers (step 8d, TASK-214.34).

Companion of cert-replay.py. See dev/design/certificate-replay.md and
dev/research/evaluations/2026-11-22-replay-covers-report.md. Registered into
cert-replay.py's REPLAYS; every function has the signature
replay(record, data, budget) -> (comparisons, why_none) of the prototype.

The original comparisons and their source references in CLAIMS are preserved
from the earlier replay. Additions read the certificates at master b6a00871.
A change to a certificate must update the corresponding replay. The comparison names do not collide with the ones of
the other modules (root, restore, moment, support, unique, face, gain, global,
concave, dispersion, multiplicity, quartic, location): the Lomax transfers reuse
the Lomax names because pareto2 and invpareto are replayed by no other module
(support is the exact predicate for pareto2 samples with a value at min; faces.py
does not replay pareto2).

Transfers (re-use of cert-replay.py's replay_lomax, bound at registration):

  pareto2    shape = kappa, scale = 1/lambda on y = x - min (binary64, as the core subtracts);
             shape held is the held-kappa root of the Lomax replay, scale held is closed form;
             samples with a value equal to min take pareto2's own branch (support); the
             historical 'mapping' information is preserved for record comparison;
             'transfer-flag' checks the current limit(false) mapping
  invpareto  shape = kappa, scale = lambda on y = 1/x (binary64, as the core divides);
             logLik differs from the Lomax value by the Jacobian -2 sum log x (double column only)
  both       coefficient: log(kappa_hat) + log A(z) within the certificate's 1e-8 over the root enclosure
             (the Lomax prototype does not check the shape coefficient)
  betapr     held unit shape uses one of the two Lomax orientations
  burr       fixed power transfers to Lomax on the powered observations

One-dimensional covers. A generic interval branch and bound (cover) closes a cell by an upper
bound of the value, by a one-signed derivative enclosure, or by two tangent lines; it never
uses a dossier bound that is not coded here as an interval:

  cauchy     scale held   bracket, curvature, cover (+ tie, n = 2)   interval
  gpd        joint, scale held   bracket, curvature, gain, cover, coefficient   interval
  foldnorm   joint        bracket, cover (profile in a = mu/sigma^2)     interval
  fatigue    shift held, scale free or alpha held   bracket, cover   interval
  skellam    both rates free      cover (profile on the mean line, Bessel by mpmath)   numerical
  paralogis  shape held (scale root), scale held (shape cover), both free (shape profile over the intercept)   interval;
             invparalogis is paralogis on 1/x (binary64 reciprocals for a certified fit, exact for an uncertified one)
  slash      mu held (scale root), sigma held (location cover), both free (location profile over the scale; accuracy by Newton, numerical)
  sgomp      rate cover with interval conditional shape roots and curvature
  dgamma     numerical log-shape cover with held rate or profiled rate
  trgamma    numerical power cover with a convex-conjugate secant, also used by invtrgamma
  huber      numerical epsilon cover with supporting-plane nuisance values

Every comparison reports a verdict (holds, fails, open) as in the prototype.
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
LOMAX_FACE_CLASS = None   # cert-replay.py's lomax_face_class, bound at registration


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
    'shape held': ('held-kappa root on log(lambda), half width 0.75e-10', 'lomax/mle.hpp:272-287,298-304 (held-kappa bracket); pareto2/mle.hpp:94-95'),
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
    'shape held': ('held-kappa root on log(lambda), half width 0.75e-10', 'lomax/mle.hpp:272-287,298-304 (held-kappa bracket); invpareto/mle.hpp:42'),
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

def replay_lomax_limit_transfer(r, data, budget):
    result = r['result']
    outcome = result.get('outcome') or {}
    expected_model, expected_mask = {'pareto2': ('exp', 4), 'invpareto': ('invexp', 0)}[r['family']]
    if r['mask'] != expected_mask or outcome.get('kind') != 'limit':
        return [], None
    if r['family'] == 'pareto2' and float(r['held'].get('min', 0)) != 0:
        # nonzero held threshold: the limit is the exponential on x - min (shift-payload 896d9dee, fixtures a529330e)
        expected_model = 'unavailable:shifted-exp'
    # A constant original sample has an analytic escape proof (lomax/certificate.hpp, TASK-238): the transfer
    # carries certified=true. Otherwise the margin-based limit stays certified=false.
    expected_certified = len({float(v) for v in data}) == 1
    model, certified = result.get('model'), outcome.get('certified')
    verdict = 'holds' if model == expected_model and certified is expected_certified else 'fails'
    flag = 'true (constant sample)' if expected_certified else 'false'
    return [comparison('transfer-flag', f'transfer the Lomax margin limit as {expected_model} with certified={flag}',
                       verdict, f'model {model}, certified {certified}', rigor='exact')], None


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

# ------------------------------------------------------------ burr, shape2 held (Lomax transfer, E)

CLAIMS['burr'] = {
    'shape2 (power c) held; shape1 and scale free': (
        'Lomax certificate on y = (x / max x)^c, shape = k, rate lambda = (max x / s)^c: root enclosure 8e-7 in z, gain 1e-8, global cover (as lomax); '
        'the Weibull limit is returned only when the strict exponential-face cover proves h(z) < 0 at every finite z (error-padded)',
        'burr/mle.hpp:375-410 (fixed power is a Lomax likelihood on exp(c (log x - max log x)); Lomax fit, scale s = exp(mx - log(lambda)/c); Weibull limit at :391); '
        'burr/lomax_face.hpp:14-52 (strict face cover); lomax/certificate.hpp:100-101,118-119'),
}

def replay_burr_power(r, data, budget):
    mask = r['mask']; res = r['result']; kind = (res.get('outcome') or {}).get('kind')
    if mask != 2 or kind not in ('estimate', 'limit'): return [], None
    c = r['held']['shape2']
    x = [mp.mpf(float(v)) for v in data]
    if not all(v > 0 for v in x): return [], 'nonpositive data'
    xmax = max(x)
    y = [float(mp.exp(mp.mpf(c)*(mp.log(v)-mp.log(xmax)))) for v in x]      # rounded to binary64, as the core forms y
    if not all(v > 0 and math.isfinite(v) for v in y): return [], 'transformed data not representable'
    n = len(x)
    jac = float(n*mp.log(c)+(c-1)*sum(mp.log(v) for v in x)-c*n*mp.log(xmax))
    if kind == 'limit':
        est = {}
    else:
        k = number(res['estimates']['shape1']); sc = number(res['estimates']['scale'])
        lam = float((xmax/mp.mpf(sc))**mp.mpf(c))
        est = dict(kappa=k, lambda_=lam)
    est2 = {('lambda' if kk == 'lambda_' else kk): v for kk, v in est.items()}
    comps, why = LOMAX(lomax_record(r, 0, {}, est2, -jac), y, budget)
    if kind == 'estimate': comps.append(lomax_shape(est2['kappa'], est2['lambda'], y))
    for cc in comps:
        cc['note'] = ('burr as lomax on (x/max x)^c, y rounded to binary64 (relative 1.1e-16 input perturbation); ' + (cc['note'] or '')).strip()
    if kind == 'limit':
        certified = res['outcome'].get('certified')
        if len(set(y)) == 1:
            comps.append(comparison('limit', 'Weibull limit: constant transformed sample (exact)', 'holds', 'h(z) < 0 for all z (exact)', rigor='exact'))
        else:
            p = LOMAX_PROFILE(y); lo, hi, _ = p.domain()
            label, text = LOMAX_FACE_CLASS(p, lo, hi, budget)
            verdict = 'holds' if label == 'none' else 'fails' if label == 'finite' else 'open'
            comps.append(comparison('limit', 'Weibull limit (certified = %s): the exponential supremum is unattained, h(z) < 0 at every finite z' % certified, verdict,
                                    text, rigor='interval', note='strict face (burr/lomax_face.hpp), not the 1e-8 margin'))
    return comps, why

# ------------------------------------------------------------ beta prime, unit held shape (E, F)

def replay_betapr_unit(r, data, budget):
    held = r['held']
    if held.get('shape1') == 1:
        shape, reflected = 'shape2', False
    elif held.get('shape2') == 1:
        shape, reflected = 'shape1', True
    else:
        return [], None
    if 'scale' in held:
        return [], 'unit shape and scale held: no profile cover'

    result = r['result']
    kind = (result.get('outcome') or {}).get('kind')
    if kind not in ('estimate', 'limit'):
        return [], None
    x = [float(v) for v in data]
    if not all(v > 0 and math.isfinite(v) for v in x):
        return [], 'unit-shape transfer requires finite positive data'
    y = [1/v for v in x] if reflected else x
    if not all(v > 0 and math.isfinite(v) for v in y):
        return [], 'unit-shape transfer has nonrepresentable reciprocals'

    mask = 2 if shape in held else 0
    lomax_held = {'kappa': held[shape]} if mask else {}
    estimates = {}
    if kind == 'estimate':
        public = result['estimates']
        scale = number(public['scale'])
        estimates = {'kappa': number(public[shape]),
                     'lambda': scale if reflected else 1/scale}
    shift = 2 * math.fsum(map(math.log, x)) if reflected else 0
    comps, why = LOMAX(lomax_record(r, mask, lomax_held, estimates, shift), y, budget)
    if kind == 'estimate' and not mask:
        comps.append(lomax_shape(estimates['kappa'], estimates['lambda'], y))
    if kind == 'limit' and len(set(y)) > 1:
        profile = LOMAX_PROFILE(y)
        lo, hi, _ = profile.domain()
        label, detail = LOMAX_FACE_CLASS(profile, lo, hi, budget)
        verdict = {'none': 'holds', 'finite': 'fails'}.get(label, 'open')
        comps.append(comparison('unit-face', 'strict exponential face on the Lomax scale',
                                verdict, detail, note='betapr/mle.hpp unit-shape face gate'))
    for comp in comps:
        route = '1/x' if reflected else 'x'
        comp['note'] = f'beta prime as Lomax on {route}. ' + comp['note']
    return comps, why


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

# ------------------------------------------------------------ gpd, shape held (C)

CLAIMS['gpd']['xi held'] = (
    'the scaled scale score s(sigma) = (1 + xi) mean(r / (sigma + xi r)) - 1 on r = (x - mu)/M, sigma in units of M, strictly decreasing, is signed on a '
    'bracket around the returned scale; the final bracket is bisected to min(tol, 1e-8) relative width (replayed at 1.1e-8, the weakest user tolerance), the '
    'representative restored within 1e-8 (the signed enclosure itself certifies 1e-7)',
    'gpd/mle.hpp:92 (bracket width min(tol, 1e-8)), :98-110 (signed box, 1e-7 mid), :124 (restoration 1e-8); README "Certificate" last paragraph')

def replay_gpd_xi_held(r, data, budget):
    res = r['result']; kind = (res.get('outcome') or {}).get('kind'); mask = r['mask']
    if kind != 'estimate' or mask != 5: return [], None
    mu = r['held']['mu']; xi = r['held']['xi']
    y = [float(v)-mu for v in data]; M = max(y)
    if not M > 0: return [], 'no positive excess'
    sigma = number(res['estimates']['sigma'])
    if xi == -1.0 or xi == 0.0: return [], 'closed form (uniform or exponential member)'
    n = len(y)
    counts = collections.Counter(v for v in y if v > 0)
    pts = [(iv.mpf(v)/iv.mpf(M), c) for v, c in sorted(counts.items())]
    xv = iv.mpf(xi)
    def score(sg):
        total = iv.mpf(0)
        for rr, c in pts:
            den = sg+xv*rr
            if lo_(den) <= 0: return None
            total += c*rr/den
        return (1+xv)*total/n-1
    s = iv.mpf(sigma)/iv.mpf(M)
    w = mp.mpf('1.1e-8')
    sh = mp.mpf(s.a)/2+mp.mpf(s.b)/2
    sl, sr = score(iv.mpf(sh*(1-w))), score(iv.mpf(sh*(1+w)))
    if sl is None or sr is None:
        return [comparison('root', 'scaled score signed on the bracket', 'open', 'denominator sigma + xi r not positive on the bracket')], None
    vl, vr = sign_verdict(sl, +1), sign_verdict(sr, -1)
    verdict = 'fails' if 'fails' in (vl, vr) else 'holds' if vl == vr == 'holds' else 'open'
    return [comparison('root', 's(sigma_hat (1 - w)) > 0 > s(sigma_hat (1 + w)), w = 1.1e-8, s strictly decreasing', verdict,
                       'left %s, right %s' % (show(sl), show(sr)), margin='32 eta (score allowance)',
                       note='held xi = %.6g; s is strictly decreasing for xi > -1 with a positive excess (dossier), so the root is the unique maximizer in scale' % xi)], None

# ------------------------------------------------------------ dgamma, rate held (F) and shape held (C), numerical

DGAMMA_MARGIN = mp.mpf('1e-8')
CLAIMS['dgamma'] = {
    'rate held, shape free': (
        'global shape maximum: branch and bound over log a on [log a_low, log a_high] with the dossier cutoffs (small shape: a <= exp(L0/w_k)/(2e log(1+1/k)) has L <= L0; '
        'large shape: a >= max(2 r (k_max+1), -L0/(n (log 2 - 1/2))) has L <= L0) and the score enclosure from the monotone conditional mean of log Y, down to the 5e-9 n margin of the '
        'search plus the 5e-9 n recomputation margin (replayed as 1e-8 per observation); the returned shape within 1e-10 relative of a root of the score',
        'dgamma/mle.hpp:233-281 (certify, margin 5e-9 at :275), :401 (returned likelihood within 5e-9 per observation of the candidate), accuracy tol 1e-10 at :419; README "Search certificate"'),
    'shape held, rate free': (
        'strictly log-concave rate likelihood: the unique root of the log-rate score a - r mean(E[Y | cell]) lies within the returned rate -/+ 1e-10 relative (accuracy contract)',
        'dgamma/mle.hpp:76-111 (inner solve), :419 (tol 1e-10, polish); README "Existence and identification" (strict concavity in log rate)'),
}

class DgammaCells:
    def __init__(self, data):
        counts = collections.Counter(int(v) for v in data)
        self.n = len(data)
        self.cells = sorted(counts.items())
    @staticmethod
    def lower(a, x):
        """Regularized lower incomplete gamma by the Kummer series, for the large-shape inputs on which mpmath's gammainc does not converge."""
        if x == 0: return mp.mpf(0)
        return mp.exp(a*mp.log(x)-x-mp.loggamma(a+1))*mp.hyp1f1(1, a+1, x, maxterms=10**7)
    def prob(self, a, r, k):
        if a > 1e5:     # mpmath's gammainc does not converge here (and fails slowly): difference of the lower tails
            v = self.lower(a, r*(k+1))-self.lower(a, r*k)
            if not v > mp.mpf(10)**(-mp.mp.dps+10): raise ValueError('cell probability below the absolute precision of the difference of lower tails')
            return v
        return mp.gammainc(a, r*k, r*(k+1), regularized=True)
    def value(self, a, r):
        return sum((mp.mpf(w)*mp.log(self.prob(a, r, k)) for k, w in self.cells), mp.mpf(0))/self.n
    def dshape(self, a, r):
        """d/da of the mean log likelihood, central difference at working precision (error ~1e-24, widened by the caller)."""
        h = a*mp.mpf('1e-12')
        total = mp.mpf(0)
        for k, w in self.cells:
            total += w*(mp.log(self.prob(a+h, r, k))-mp.log(self.prob(a-h, r, k)))/(2*h)
        return total/self.n
    def rate_score(self, a, r):
        """d/d log r of the mean log likelihood: a - r mean E[Y | cell] = a (1 - mean ratio)."""
        total = mp.mpf(0)
        for k, w in self.cells:
            ratio = self.prob(a+1, r, k)/self.prob(a, r, k)
            total += w*ratio
        return a*(1-total/self.n)

def dgamma_cutoffs(cells, L0, r):
    """Shape interval [a_low, a_high] outside which L <= L0 (total log likelihood level), from the dossier bounds. r is None for a free rate."""
    n = cells.n
    low = mp.mpf(0)
    for k, w in cells.cells:
        if k > 0: low = max(low, min(mp.mpf(1), mp.exp(L0/w)/(2*mp.e*mp.log1p(mp.mpf(1)/k))))
    if r is not None:
        high = max(2*r*(cells.cells[-1][0]+1), -L0/(n*(mp.log(2)-mp.mpf(1)/2)))
    else:
        high = mp.inf
    return low, high

def replay_dgamma(r, data, budget):
    try: return replay_dgamma_run(r, data, budget)
    except (mp.libmp.libhyper.NoConvergence, ZeroDivisionError, ValueError, OverflowError) as error:
        return [comparison('cover' if r['mask'] == 2 else 'bracket', 'evaluation raised', 'open', 'mpmath incomplete gamma: %s: %s' % (type(error).__name__, error), rigor='numerical',
                           note='the 50-digit evaluation failed at these coefficients')], None

def replay_dgamma_run(r, data, budget):
    res = r['result']; kind = (res.get('outcome') or {}).get('kind'); mask = r['mask']
    if kind != 'estimate' or mask not in (1, 2): return [], None
    cells = DgammaCells(data)
    if all(k == 0 for k, w in cells.cells): return [], 'all zero: point mass'
    ahat = mp.mpf(number(res['estimates']['shape'])); rhat = mp.mpf(number(res['estimates']['rate']))
    out = []
    w = mp.mpf('1e-10')
    if mask == 1:
        gl, gr = cells.rate_score(ahat, rhat*(1-w)), cells.rate_score(ahat, rhat*(1+w))
        verdict = 'holds' if gl > 0 > gr else 'fails' if gl < 0 and gr < 0 or gl > 0 and gr > 0 else 'open'
        if gl < 0 < gr: verdict = 'fails'
        out.append(comparison('bracket', "log-rate score signed on r_hat (1 -/+ 1e-10): positive left, negative right", verdict,
                              'left %s, right %s' % (mp.nstr(gl, 8), mp.nstr(gr, 8)), margin='1e-11 min(1, a) per observation (solver)', rigor='numerical',
                              note='mpmath incomplete gamma at 50 digits; the log-rate likelihood is strictly concave (dossier), so the root is the unique maximizer'))
        return out, None
    # rate held: shape free
    ll_hat = cells.value(ahat, rhat); target = ll_hat+DGAMMA_MARGIN
    # bracket on the score at a_hat (1 -/+ 1e-10)
    dl, dr = cells.dshape(ahat*(1-w), rhat), cells.dshape(ahat*(1+w), rhat)
    allowance = mp.mpf('1e-15')
    vb = 'holds' if dl-allowance > 0 > dr+allowance else 'fails' if dl+allowance < 0 or dr-allowance > 0 else 'open'
    out.append(comparison('bracket', "shape score signed on a_hat (1 -/+ 1e-10): positive left, negative right", vb,
                          'left %s, right %s' % (mp.nstr(dl, 8), mp.nstr(dr, 8)), margin='1e-15 numerical-difference allowance', rigor='numerical',
                          note='central difference of the log cell probabilities at 50 digits'))
    L0 = cells.n*target
    low, high = dgamma_cutoffs(cells, L0, rhat)
    if low <= 0:    # no positive cell; excluded above
        return out, None
    if not high > low: return out+[comparison('cover', 'global shape maximum', 'open', 'cutoffs cross: low %s high %s' % (mp.nstr(low, 6), mp.nstr(high, 6)))], None
    cache = {}
    def lshape(a):
        if a not in cache: cache[a] = cells.dshape(a, rhat)
        return cache[a]
    def value(t): return iv.mpf(cells.value(mp.exp(t), rhat))
    def deriv(ta, tb):
        A, B = mp.exp(ta), mp.exp(tb)
        dA, dB = lshape(A), lshape(B)
        pA, pB = mp.digamma(A), mp.digamma(B)
        lo_s = dA+pA-pB-allowance; hi_s = dB+pB-pA+allowance          # L_a/n in [L_a(A) + psi(A) - psi(B), L_a(B) + psi(B) - psi(A)]
        lo_t = lo_s*(B if lo_s < 0 else A); hi_t = hi_s*(B if hi_s > 0 else A)    # f_t = a L_a, a in [A, B]
        return iv.mpf([lo_t, hi_t])
    verdict, info = cover(value, deriv, None, mp.log(low), mp.log(high), target, [], budget=max(budget//4, 400))
    out.append(cover_comparison('cover', 'f(a) < f(a_hat) + 1e-8 per observation for every a in [%s, %s] (cutoffs: small and large shape bounds of the dossier)' % (
        mp.nstr(low, 6), mp.nstr(high, 6)), verdict, info, target, rigor='numerical', extra='; cutoffs in a: [%s, %s]' % (mp.nstr(low, 6), mp.nstr(high, 6)),
        note='target is the mean log likelihood at the returned shape plus 1e-8; variable log a; derivative from the monotone conditional mean of log Y (numerical difference)'))
    return out, None

def replay_dgamma_joint(r, data, budget):
    """Joint cover in log shape with the concave rate optimum bounded at each point."""
    result = r['result']
    outcome = result.get('outcome') or {}
    if r['mask'] or outcome.get('kind') != 'estimate' or not outcome.get('certified'):
        return [], None
    claim = 'joint shape profile within 1e-8 per observation of the returned likelihood'
    try:
        cells = DgammaCells(data)
        shape = mp.mpf(number(result['estimates']['shape']))
        rate = mp.mpf(number(result['estimates']['rate']))
        cutoff = cells.n*cells.value(shape, rate)
        target = cutoff/cells.n+mp.mpf('1e-8')
        low, _ = dgamma_cutoffs(cells, cutoff, None)
        high = mp.inf
        for i, (l, w) in enumerate(cells.cells):
            for h, count in cells.cells[i+1:]:
                if h > l+1:
                    logq = mp.log(mp.mpf(h)/(l+1))/2
                    c = min(mp.expm1(logq)-logq, mp.expm1(-logq)+logq)
                    high = min(high, -cutoff/(c*min(w, count)))
        if not (0 < low < high < mp.inf):
            raise ValueError('small and large shape bounds did not give a compact domain')
        mean = mp.mpf(sum(k*w for k, w in cells.cells))/cells.n

        def integral(a, rate):
            return cells.value(a, rate)+mp.loggamma(a)-a*mp.log(rate)

        def rate_terms(a, rate):
            # Differentiate the two CDF endpoints with respect to log rate.
            # Both score and curvature then need just one cell probability.
            normalizer = mp.loggamma(a)
            scores, slopes, values = [], [], []
            endpoints = {0: mp.mpf(0)}

            def endpoint(k):
                if k not in endpoints:
                    z = rate*k
                    endpoints[k] = mp.exp(a*mp.log(z)-z-normalizer)
                return endpoints[k]

            for k, count in cells.cells:
                probability = cells.prob(a, rate, k)
                left, right = endpoint(k), endpoint(k+1)
                score = (right-left)/probability
                slope = ((a-rate*(k+1))*right-(a-rate*k)*left)/probability-score*score
                scores.append(count*score)
                slopes.append(count*slope)
                values.append(count*mp.log(probability))
            return tuple(mp.fsum(v)/cells.n for v in (scores, slopes, values))

        pending = [(mp.log(low), mp.log(high))]
        count = 0
        while pending and count < budget:
            count += 1
            tl, th = pending.pop()
            mid = (tl+th)/2
            a, b, shape = mp.exp(tl), mp.exp(th), mp.exp(mid)
            rl, rh = mp.log(shape/(mean+1)), mp.log(shape/mean)
            u = (rl+rh)/2
            for _ in range(96):
                rate = mp.exp(u)
                score, slope, value = rate_terms(shape, rate)
                nuisance_error = max(score*(rl-u), score*(rh-u))
                if nuisance_error < mp.mpf('1e-12'):
                    break
                if score > 0:
                    rl = u
                else:
                    rh = u
                newton = u-score/slope if slope < 0 else u
                u = newton if rl < newton < rh else (rl+rh)/2
            else:
                raise ValueError('conditional rate bracket did not close')
            if value > target:
                return [comparison('joint-cover', claim, 'fails',
                                   'shape %s, rate %s, likelihood %s, target %s' %
                                   tuple(mp.nstr(x, 20) for x in (shape, rate, value, target)),
                                   rigor='numerical')], None

            rl, rh = mp.log(a/(mean+1)), mp.log(b/mean)
            for _ in range(96):
                rr = (rl+rh)/2
                rate = mp.exp(rr)
                if rate_terms(b, rate)[0] > b-a:
                    rl = rr
                elif rate_terms(a, rate)[0] < a-b:
                    rh = rr
                else:
                    break
            h = mp.mpf('1e-4')*a
            rlo, rhi = mp.exp(rl), mp.exp(rh)
            lower = rl-mp.digamma(b)+(integral(a, rhi)-integral(a-h, rhi))/h
            upper = rh-mp.digamma(a)+(integral(b+h, rlo)-integral(b, rlo))/h
            if lower > upper:
                raise ValueError('conditional log-cell secants crossed')
            rise = max(0, -b*lower*(mid-tl), b*upper*(th-mid))
            if value+nuisance_error+rise < target:
                continue
            if not tl < mid < th:
                raise ValueError('shape interval collapsed at working precision')
            pending.extend(((tl, mid), (mid, th)))
        if not pending:
            return [comparison('joint-cover', claim, 'holds',
                               f'target {mp.nstr(target, 20)}. {count} cells closed',
                               rigor='numerical')], None
        detail = f'{count} cells checked without closing the cover. Target {mp.nstr(target, 20)}'
    except (mp.libmp.libhyper.NoConvergence, ValueError, ZeroDivisionError, OverflowError) as error:
        detail = str(error)
    return [comparison('joint-cover', claim, 'open', detail, rigor='numerical',
                       note='mpmath incomplete gamma with analytic shape and rate brackets')], None


# ------------------------------------------------------------ ht, shape free (F, numerical)

HT_MARGIN = mp.mpf('1e-8')
CLAIMS['ht'] = {
    'shape free (endpoint held or free = minimum; scale held or free)': (
        'the returned finite candidate is within 1e-8 n of the supremum of the log likelihood: branch and bound over t = log(nu) with the scale roots enclosed over each cell '
        '(monotone contributions of the positive residuals), the profile score enclosed by [r(w-) - b h(w+)]/2 and [r(w+) - a h(w-)]/2 plus the digamma-free or digamma bounds for the '
        'normalizer, the small-shape bound n/2 log(2a/pi) - sum log d - k log(sigma held) (endpoint held) or the restriction nu >= 1 (endpoint free), and the large-shape bound '
        'l_HN + (1 + 1/a)/(4 nu) sum (d/s0)^4 with s0 = sigma held or d_min sqrt((a p - k)/(a n)) at a = 64',
        'ht/cert.hpp:72 (eps = 1e-8 n; comparisons at incumbent + eps/4: :144, :214, :218), :17-66 (scale box), :129-148 (small shape and tie neighbourhood), :152-162 (large shape), :172-218 (cells, score enclosure); README "Explicit tail and inner-solve bounds", "A computable interval score enclosure"'),
}

class HtProfile:
    """Half-t with endpoint-shifted positive residuals d, k endpoint ties, n observations; log likelihood in (nu, u = log sigma)."""
    def __init__(self, d, held_sigma=None):
        self.n = len(d); self.pos = sorted(v for v in d if v > 0); self.p = len(self.pos); self.k = self.n-self.p
        self.logs = [mp.log(mp.mpf(v)) for v in self.pos]
        self.held_u = None if held_sigma is None else mp.log(mp.mpf(held_sigma))
        self.sumlog = sum(self.logs, mp.mpf(0))
    @staticmethod
    def logmode(nu): return mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(mp.pi*nu)/2
    def score_u(self, nu, u):
        return sum(((nu+1)/(1+nu*mp.exp(2*(u-l))) for l in self.logs), mp.mpf(0))-self.n
    def root_u(self, nu):
        """Root of the (strictly decreasing) log-scale score at shape nu by bisection at working precision."""
        al = self.p-self.k/nu
        if not al > 0: return None
        lo = min(self.logs)+mp.log(al/self.n)/2-1; hi = max(self.logs)+2
        # the extreme-distance bracket of the dossier, widened; the signs are verified and the ends expanded if needed
        for _ in range(60):
            if self.score_u(nu, lo) > 0: break
            lo -= 1
        for _ in range(60):
            if self.score_u(nu, hi) < 0: break
            hi += 1
        for _ in range(250):
            mid = (lo+hi)/2
            if self.score_u(nu, mid) > 0: lo = mid
            else: hi = mid
            if hi-lo < mp.mpf(10)**(-mp.mp.dps+8): break
        return (lo+hi)/2
    def u_at(self, nu):
        return self.held_u if self.held_u is not None else self.root_u(nu)
    def loglik(self, nu, u):
        total = self.n*(mp.log(2)+self.logmode(nu)-u)
        for l in self.logs: total -= (nu+1)*mp.log1p(mp.exp(2*(l-u))/nu)/2
        return total
    def profile(self, nu):
        u = self.u_at(nu)
        return None if u is None else self.loglik(nu, u)
    def score_t(self, nu, u):
        """d/dt of the log likelihood at (nu, u): n nu A'(nu) - (nu/2) sum log(1 + w) + ((nu+1)/2) sum w/(1+w)."""
        nuA = nu*(mp.digamma((nu+1)/2)-mp.digamma(nu/2)-1/nu)/2
        total = self.n*nuA
        for l in self.logs:
            w = mp.exp(2*(l-u))/nu
            total += -nu*mp.log1p(w)/2+(nu+1)*w/(1+w)/2
        return total
    def scale_box(self, a, b):
        """Bounds on log sigma* for every nu in [a, b] (dossier: the score contributions are monotone in nu, sign by distance)."""
        if self.held_u is not None: return self.held_u, self.held_u
        al, bl = self.p-self.k/a, self.p-self.k/b
        if not (al > 0 and bl > 0): return None
        def total(u, upper):
            v = -mp.mpf(self.n)
            for l in self.logs:
                x = (a+1)/(1+a*mp.exp(2*(u-l))); y = (b+1)/(1+b*mp.exp(2*(u-l)))
                v += max(x, y) if upper else min(x, y)
            return v
        def solve(upper):
            lo = min(self.logs)+mp.log(al/self.n)/2-1; hi = max(self.logs)+mp.log(bl/self.n)/2+1
            for _ in range(60):
                if total(lo, upper) > 0: break
                lo -= 1
            for _ in range(60):
                if total(hi, upper) < 0: break
                hi += 1
            for _ in range(250):
                mid = (lo+hi)/2
                if total(mid, upper) > 0: lo = mid
                else: hi = mid
                if hi-lo < mp.mpf(10)**(-mp.mpf(mp.mp.dps)+8): break
            return lo, hi
        l_lo, l_hi = solve(False); u_lo, u_hi = solve(True)
        return l_lo, u_hi       # lower root of the lower score, upper root of the upper score (outer ends of the root brackets)
    def score_enclosure(self, a, b):
        """Enclosure (lo, hi) of the profile score d l / dt on t in [log a, log b]; None if the scale roots are not enclosed."""
        box = self.scale_box(a, b)
        if box is None: return None
        ul, uh = box
        if a >= 32:
            low = 1/(4*b)-1/(8*a**3); high = 1/(4*a)-1/(8*b**3)+1/(4*a**5)
        else:
            low = a/2*max(mp.mpf(0), mp.digamma((a+1)/2)-mp.digamma(b/2))-mp.mpf(1)/2
            high = b/2*(mp.digamma((b+1)/2)-mp.digamma(a/2))-mp.mpf(1)/2
        low *= self.n; high *= self.n
        for l in self.logs:
            wl = mp.exp(2*(l-uh))/b; wh = mp.exp(2*(l-ul))/a       # w = d^2/(nu sigma^2) in [wl, wh]
            rl, rh = wl/(1+wl), wh/(1+wh)
            hl, hh = mp.log1p(wl)-rl, mp.log1p(wh)-rh
            low += (rl-b*hh)/2; high += (rh-a*hl)/2
        return low, high

def replay_ht(r, data, budget):
    try: return replay_ht_run(r, data, budget)
    except (ZeroDivisionError, ValueError, OverflowError, mp.libmp.libhyper.NoConvergence) as error:
        return [comparison('cover', 'evaluation raised', 'open', 'ht evaluation: %s: %s' % (type(error).__name__, error), rigor='numerical',
                           note='the 50-digit evaluation failed at these coefficients')], None

def replay_ht_run(r, data, budget):
    res = r['result']; kind = (res.get('outcome') or {}).get('kind'); mask = r['mask']
    if kind != 'estimate' or mask not in (0, 1, 4, 5): return [], None
    free_mu = not mask & 1; held_sigma = r['held'].get('sigma') if mask & 4 else None
    x = [float(v) for v in data]
    mu = min(x) if free_mu else r['held']['mu']
    d = [v-mu for v in x]
    if any(v < 0 for v in d): return [], 'observations below the held endpoint'
    p = HtProfile(d, held_sigma)
    if p.p == 0: return [], 'all observations at the endpoint'
    n = p.n
    tie = free_mu and held_sigma is None and 2*p.k == n
    if tie: return [], 'exact tie 2k = n with free endpoint and scale (scale-zero face): not replayed'
    if p.held_u is None and p.k > 0 and not free_mu: return [], 'endpoint ties with free shape and scale: unbounded (stop)'
    nu_hat = mp.mpf(number(res['estimates']['nu'])); sigma_hat = mp.mpf(number(res['estimates']['sigma']))
    l_hat = p.loglik(nu_hat, mp.log(sigma_hat)); target = l_hat/n+HT_MARGIN; L0 = n*target
    out = []
    at_bound = free_mu and nu_hat <= 1
    if not at_bound:
        w = mp.mpf('1e-10')
        sl = p.score_t(nu_hat*(1-w), p.u_at(nu_hat*(1-w))); sr = p.score_t(nu_hat*(1+w), p.u_at(nu_hat*(1+w)))
        out.append(comparison('bracket', "profile score in log nu signed on nu_hat (1 -/+ 1e-10): positive left, negative right",
                              'holds' if sl > 0 > sr else 'fails' if sl < 0 and sr < 0 or sl > 0 and sr > 0 else 'open',
                              'left %s, right %s' % (mp.nstr(sl, 8), mp.nstr(sr, 8)), margin='accuracy contract tol 1e-10', rigor='numerical',
                              note='scale root by bisection at 50 digits; shape score from digamma at 50 digits'))
    # shape interval: [t_lo, t_hi]
    if free_mu: t_lo = mp.mpf(0)
    else:
        sigma_term = p.k*p.held_u if p.held_u is not None else mp.mpf(0)
        t_lo = mp.log(mp.pi/2)+2*(L0+p.sumlog+sigma_term)/n          # n/2 log(2a/pi) - sum log d - k log(sigma) <= L0 for log a below
        if t_lo > 0: t_lo = mp.mpf(0)
        if t_lo < -700: return out+[comparison('cover', 'global shape maximum', 'open', 'small-shape cutoff below exp(-700)')], None
    aa = mp.mpf(64)
    if p.held_u is not None: s0 = mp.exp(p.held_u)
    else:
        if not aa*p.p > p.k: return out+[comparison('cover', 'global shape maximum', 'open', 'a p <= k')], None
        s0 = mp.mpf(p.pos[0])*mp.sqrt((aa*p.p-p.k)/(aa*n))
    # half-normal supremum of the log likelihood (scale root, or the held scale)
    m2 = sum((mp.mpf(v)**2 for v in p.pos), mp.mpf(0))
    sig2 = (m2/n) if p.held_u is None else mp.exp(2*p.held_u)
    l_hn = n*(mp.log(2)-mp.log(2*mp.pi)/2-mp.log(sig2)/2)-m2/(2*sig2)
    gap = L0-l_hn
    if not gap > 0:
        return out+[comparison('cover', 'global shape maximum', 'fails' if l_hn > l_hat+0 else 'open',
                               'half-normal face %s not below the target %s' % (mp.nstr(l_hn, 15), mp.nstr(L0, 15)), rigor='numerical')], None
    corr_sum = sum(((mp.mpf(v)/s0)**4 for v in p.pos), mp.mpf(0))
    D = (1+1/aa)*corr_sum/(4*gap)                # nu >= max(D, a) has sup l <= l_hn + correction < target
    t_hi = mp.log(max(D, aa))*(1+mp.mpf('1e-12'))
    if t_hi > 90: return out+[comparison('cover', 'global shape maximum', 'open', 'large-shape cutoff exp(%s) beyond the evaluated range' % mp.nstr(t_hi, 5))], None
    if not t_hi > t_lo: t_hi = t_lo+1
    def value(t):
        v = p.profile(mp.exp(t))
        if v is None: raise ValueError('scale root not enclosed')
        return iv.mpf(v/n)
    def deriv(ta, tb):
        e = p.score_enclosure(mp.exp(ta), mp.exp(tb))
        if e is None: return iv.mpf([-mp.inf, mp.inf])
        return iv.mpf([e[0]/n, e[1]/n])
    verdict, info = cover(value, deriv, None, t_lo, t_hi, target, [], budget=budget*5)
    out.append(cover_comparison('cover', 'l(nu, sigma) < l_hat + 1e-8 n for every nu in [%s, %s] (profile in sigma; cutoffs of the dossier outside)' % (
        mp.nstr(mp.exp(t_lo), 6), mp.nstr(mp.exp(t_hi), 6)), verdict, info, target, rigor='numerical',
        extra='; nu range [%s, %s], half-normal face %s' % (mp.nstr(mp.exp(t_lo), 6), mp.nstr(mp.exp(t_hi), 6), mp.nstr(l_hn/n, 12)),
        note='variable log nu; profile score enclosed per cell by the dossier bounds; values by mpmath loggamma at 50 digits'))
    return out, None

def replay_ht_face(r, data, budget):
    result = r['result']
    outcome = result.get('outcome') or {}
    mask = r['mask']
    normal = outcome.get('kind') == 'limit' and result.get('model') == 'hnorm'
    collapse = outcome.get('kind') == 'nonexistence' and outcome.get('reason') == 'unattained'
    if mask not in (0, 1, 4, 5) or not (normal or collapse):
        return [], None
    claim = 'selected half-t face within 2.5e-9 per observation of the entire shape profile'
    free_endpoint = not mask & 1
    x = [float(v) for v in data]
    if not x:
        return [], None
    endpoint = min(x) if free_endpoint else r['held']['mu']
    data = [v-endpoint for v in x]
    if min(data) < 0 or not max(data) > 0:
        return [], 'degenerate sample or outside support'
    unit = max(data)
    held_scale = r['held'].get('sigma')
    try:
        # Center the scale coordinate so the shape tail bounds do not depend
        # on the physical units of the observations.
        p = HtProfile([mp.mpf(v)/unit for v in data],
                      None if held_scale is None else mp.mpf(held_scale)/unit)
        tie = free_endpoint and held_scale is None and 2*p.k == p.n
        if collapse and not tie:
            return [], 'unattained outcome is outside the equal-mass face cover'
        collapse_value = p.n*mp.log(2/mp.pi)-2*p.sumlog if tie else mp.ninf
        m2 = mp.fsum(v*v for v in p.pos)
        variance = m2/p.n if held_scale is None else mp.exp(2*p.held_u)
        normal_value = p.n*(mp.log(2)-mp.log(2*mp.pi*variance)/2)-m2/(2*variance)
        base, rival = (normal_value, collapse_value) if normal else (collapse_value, normal_value)
        allowance = mp.mpf('2.5e-9')*p.n
        target = base+allowance
        if rival > target:
            return [comparison('face-cover', claim, 'fails',
                               f'competing face {mp.nstr(rival, 20)} above target {mp.nstr(target, 20)}',
                               rigor='numerical')], None

        if not free_endpoint:
            fixed_term = p.k*p.held_u if held_scale is not None else 0
            lo = min(-4, 2*(base-1+p.sumlog+fixed_term)/p.n-mp.log(2/mp.pi)-1)
        elif tie:
            delta = mp.mpf('0.5')
            for _ in range(100):
                bound = (collapse_value+(p.n+p.p*(2+delta))*mp.log1p(delta)/2
                         +delta*abs(p.sumlog)+p.p*delta*max(p.logs[-1], 0))
                if bound < target:
                    break
                delta /= 2
            else:
                raise ValueError('scale-zero neighborhood overlaps the face threshold')
            lo = mp.log1p(delta)
        else:
            lo = mp.mpf(0)

        aa = 64
        if held_scale is not None:
            scale_floor = mp.exp(p.held_u)
        else:
            c = (3*p.p+3)//4
            if (aa+1)*c <= p.n:
                raise ValueError('no positive lower scale bound in the normal tail')
            scale_floor = p.pos[-c]*mp.sqrt(mp.mpf((aa+1)*c-p.n)/(aa*p.n))
        tail = (1+mp.mpf(1)/aa)*mp.fsum((v/scale_floor)**4 for v in p.pos)/4
        if normal_value >= target:
            raise ValueError('normal tail overlaps the face threshold')
        hi = mp.log(max(aa, 2*tail/(target-normal_value)))
        if not -700 <= lo < hi <= 90:
            raise ValueError(f'compact log-shape interval {mp.nstr(lo, 8)}, {mp.nstr(hi, 8)} exceeds the numerical range')

        def value(t):
            likelihood = p.profile(mp.exp(t))
            if likelihood is None:
                raise ValueError('conditional scale score has no interior root')
            return iv.mpf(likelihood)

        def derivative(a, b):
            pair = p.score_enclosure(mp.exp(a), mp.exp(b))
            return iv.mpf(pair if pair else [-mp.inf, mp.inf])

        verdict, detail = cover(value, derivative, None, lo, hi, target, budget=budget)
        witness, where = detail['best']
        hp = f'target {mp.nstr(target, 20)}. Best witness {mp.nstr(witness, 20)} at log shape {mp.nstr(where, 16)}'
        if verdict == 'open':
            hp += '. ' + detail['reason']
        if verdict == 'holds' and collapse and normal_value >= base-allowance:
            verdict = 'open'
            hp += '. Normal-face priority within the comparison margin depends on the missing interior maximizer'
        return [comparison('face-cover', claim, verdict, hp, rigor='numerical',
                           note=f'{detail["evaluations"]} evaluations. Likelihood in max-excess units')], None
    except (ValueError, ZeroDivisionError, OverflowError, mp.libmp.libhyper.NoConvergence) as error:
        return [comparison('face-cover', claim, 'open', str(error), rigor='numerical')], None


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
    if (res.get('outcome') or {}).get('certified') is False:
        # An uncertified outcome is listed, not judged (design: verdicts); e.g. twin reciprocal
        # roots tied within the certificate's 1e-10 n margin, both returned as candidates.
        for comp in out:
            if comp.get('verdict') == 'fails':
                comp['verdict'] = 'open'; comp['note'] = ((comp.get('note') or '')+'; uncertified outcome (unique = %s): listed, not judged' % (res.get('outcome') or {}).get('unique')).lstrip('; ')
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

# ------------------------------------------------------------ paralogis and invparalogis (E, F)

CLAIMS['paralogis'] = {
    'shape held, scale free': ('the log-scale score a n - ... strictly decreasing in the intercept b = -a log(theta): L_b = n - (a+1) sum sigmoid(a y_i + b) has one root, within theta (1 -/+ 1e-10) of the returned scale '
                               '(concave in b: the root is the unique maximizer)',
                               'paralogis/mle.hpp:380-382 (paralogis_mle_holds delegates to burr_mle_holds), burr/mle.hpp:343-353 (bisection to the double midpoint), :414,419; README "Details" (strictly monotone log-scale score)'),
    'scale held, shape free': ('global shape maximum of L(a) = 2n log a + a sum y_i - (a+1) sum softplus(a y_i), y_i = log x_i - log(scale): L <= 2n log a at the small end, '
                               'L <= 2n log a - d a for a >= 1 (d = max |y_i| > 0) at the large end (replayed cutoffs), interval cover in log a between, window around the returned shape; '
                               'stationary signed at the returned shape (1 -/+ 1e-10)',
                               'paralogis/mle.hpp:340-356 (held-scale certify), certificate.hpp (compact cover, shape radius 1e-8 a, tail cutoffs); README "Global certificate" (held scale paragraph)'),
    'both free': ('global maximum of L(a, b) = 2n log a + sum(a y_i + b) - (a+1) sum softplus(a y_i + b): the profile P(a) = max_b L(a, b) (L_b strictly decreasing, root enclosed '
                  'by interval sign checks, P\' = L_a at the root by the envelope theorem) is below L(returned) outside a window around the returned shape; small end L <= n log a, large end L <= n log(4^(3/5) a) - (3/5) a Delta; '
                  'both profile signs at the returned shape (1 -/+ 1e-10) and the scale score at the returned scale (1 -/+ 1e-10)',
                  'paralogis/mle.hpp:318-327 (certify), certificate.hpp (box fixed point, compact cover, shape radius 1e-8 a, tail cutoffs); README "Global certificate"'),
    'constant sample': ('constant positive samples with both coefficients free have unbounded likelihood (v = y0 + log(a)/a gives density ~ a/e): exact predicate',
                        'paralogis/mle.hpp:313-318 (the constant sample returns Unbounded); README "Existence and identification" and "Global certificate" first paragraphs'),
}
CLAIMS['invparalogis'] = {
    'transfer': ('paralogis on y = 1/x (binary64 reciprocals as the core divides for a certified fit; exact reciprocals for an uncertified fit, which the core polishes on the original x), same shape, scale theta_p = 1/scale (a held scale as the binary64 reciprocal the core passes)',
                 'invparalogis/mle.hpp:55-64, :99-102 (reciprocals), :69 (physical = 1/scale), :70-82 (uncertified: polished on the original x), :126-127 (holds)'),
}

class ParaLik:
    """Paralogistic log likelihood without the data Jacobian: L(a, b) = 2n log a + sum z_i - (a+1) sum log(1 + e^z_i), z_i = a y_i + b.
    ys: interval (iv) or mpf logs; the numeric kind picks exp and log. Scores: L_a, L_b by the formulas of the dossier."""
    def __init__(self, ys, kind):
        self.kind = kind; self.n = len(ys)
        self.exp = iv.exp if kind == 'iv' else mp.exp
        self.log = iv.log if kind == 'iv' else mp.log
        self.ys = list(ys)
        self.sum_y = sum(self.ys, self.zero())
    def zero(self): return iv.mpf(0) if self.kind == 'iv' else mp.mpf(0)
    def parts(self, a, b):
        """sums of sigmoid, y sigmoid, softplus, z over the sample"""
        ssig = self.zero(); sysig = self.zero(); ssp = self.zero(); sz = self.zero()
        for y in self.ys:
            z = a*y+b
            e = self.exp(-z)
            sg = 1/(1+e)
            ssig += sg; sysig += y*sg; ssp += self.log(1+self.exp(z)); sz += z
        return ssig, sysig, ssp, sz
    def L(self, a, b):
        ssig, sysig, ssp, sz = self.parts(a, b)
        return 2*self.n*self.log(a)+sz-(a+1)*ssp
    def La(self, a, b):
        ssig, sysig, ssp, sz = self.parts(a, b)
        return 2*self.n/a+self.sum_y-ssp-(a+1)*sysig
    def Lb(self, a, b):
        ssig = self.parts(a, b)[0]
        return self.n-(a+1)*ssig

def para_hull(p, a):
    """Bounds of every stationary intercept at shape a (dossier: sum sigmoid = n/(a+1) forces -log a - a max y <= b <= -log a - a min y), as mpf (a: mpf)."""
    ymax = max(p.ys); ymin = min(p.ys)
    return -mp.log(a)-a*ymax, -mp.log(a)-a*ymin

def para_root_b(pm, a):
    """Approximate root of L_b(a, .) in plain mpmath by safeguarded Newton on the hull (a: mpf). pm: ParaLik of mpf logs."""
    lo, hi = para_hull(pm, a); lo -= 1; hi += 1
    b = (lo+hi)/2
    for _ in range(200):
        g = pm.Lb(a, b)
        if g > 0: lo = b
        else: hi = b
        q = sum((lambda s: s*(1-s))(1/(1+mp.exp(-(a*y+b)))) for y in pm.ys)
        step = g/((a+1)*q) if q > 0 else mp.inf
        nb = b+step
        if not (lo < nb < hi): nb = (lo+hi)/2
        if abs(nb-b) < mp.mpf(10)**(-mp.mp.dps+6)*(1+abs(b)): b = nb; break
        b = nb
    return b

def para_enclose_b(pi, pm, A, width_hint):
    """Intercepts (blo, bhi) with L_b(a, blo) > 0 > L_b(a, bhi) certain for every a in the interval A; None if not found."""
    am = (mp.mpf(A.a)+mp.mpf(A.b))/2
    b0 = para_root_b(pm, am)
    ymaxabs = max(max(abs(mp.mpf(y.a)), abs(mp.mpf(y.b))) for y in pi.ys)
    kappa = (mp.mpf(10)**-25+mp.mpf('0.1')*width_hint*(1+ymaxabs))*(1+abs(b0))
    for _ in range(120):
        blo, bhi = b0-kappa, b0+kappa
        gl, gh = pi.Lb(A, iv.mpf(blo)), pi.Lb(A, iv.mpf(bhi))
        if lo_(gl) > 0 and hi_(gh) < 0: return blo, bhi, hi_(gl), lo_(gh)
        kappa *= 4
    return None

def para_value(v):
    """interval of a data value: a binary64 value, or ('inv', v) for the exact reciprocal 1/v"""
    return iv.mpf(1)/iv.mpf(v[1]) if isinstance(v, tuple) else iv.mpf(v)

class ParaProfile:
    """Profile P(a) = max_b L(a, b) with enclosures (see CLAIMS['paralogis'], both free)."""
    def __init__(self, x, center):
        """logs centered at `center` (the returned log scale): b' = b + a center, the profile in a is unchanged (a bijection of b at each a, envelope theorem in b')"""
        c = iv.mpf(center)
        self.pi = ParaLik([iv.log(para_value(v))-c for v in x], 'iv')
        self.pm = ParaLik([(lambda w: mp.mpf(w.a)/2+mp.mpf(w.b)/2)(iv.log(para_value(v))-c) for v in x], 'mp')
        self.n = self.pi.n
    def value(self, t):
        a = iv.exp(iv.mpf(t))
        e = para_enclose_b(self.pi, self.pm, a, mp.mpf(0))
        if e is None: raise ValueError('intercept not enclosed')
        blo, bhi, glo, ghi = e
        bm = iv.mpf(blo)/2+iv.mpf(bhi)/2
        v = self.pi.L(a, bm)
        err = max(glo, -ghi)*(bhi-blo)          # concave in b: L(b*) - L(bm) <= max |L_b| |b* - bm|
        return iv.mpf([lo_(v), hi_(v)+err])
    def dt(self, ta, tb):
        """Enclosure of d P / d t, t = log a, on [ta, tb]: a L_a(a, b*(a)) by the envelope theorem."""
        A = iv.exp(iv.mpf([ta, tb]))
        e = para_enclose_b(self.pi, self.pm, A, mp.mpf(tb)-mp.mpf(ta))
        if e is None: return iv.mpf([-mp.inf, mp.inf])
        blo, bhi, _, _ = e
        return A*self.pi.La(A, iv.mpf([blo, bhi]))
    def score_at(self, a):
        """enclosure of P'(a) at an mpf shape"""
        A = iv.mpf(a)
        e = para_enclose_b(self.pi, self.pm, A, mp.mpf(0))
        if e is None: return None
        return self.pi.La(A, iv.mpf([e[0], e[1]]))

class ParaHeld:
    """Held scale: y_i = log x_i - log(scale), b = 0; L(a) = 2n log a + a sum y - (a+1) sum softplus(a y)."""
    def __init__(self, x, scale):
        u = iv.log(iv.mpf(scale))
        self.pi = ParaLik([iv.log(para_value(v))-u for v in x], 'iv')
        self.n = self.pi.n
    def value(self, t):
        a = iv.exp(iv.mpf(t)); return self.pi.L(a, iv.mpf(0))
    def dt(self, ta, tb):
        A = iv.exp(iv.mpf([ta, tb])); return A*self.pi.La(A, iv.mpf(0))
    def score_at(self, a): return self.pi.La(iv.mpf(a), iv.mpf(0))

def para_cutoffs(p, L0, held):
    """Shape interval [a_lo, a_hi] outside which L <= L0 (dossier tail bounds; free: small L <= n log a, large L <= n log(4^(3/5) a) - (3/5) a Delta for a >= 1;
    held: small L <= 2n log a, large L <= 2n log a - d a for a >= 1). Returns (lo, hi) or (None, why)."""
    n = p.n
    if held:
        d = max(abs(mp.mpf(y.a)) if abs(mp.mpf(y.a)) > abs(mp.mpf(y.b)) else abs(mp.mpf(y.b)) for y in p.pi.ys)
        lo = mp.exp(L0/(2*n))
        if not d > 0: return None, 'all x equal to the held scale (exact likelihood, not replayed)'
        bound = lambda a: 2*n*mp.log(a)-d*a
        knee = 2*n/d
    else:
        # Delta is the spread of the log data; a lower enclosure suffices (smaller Delta, weaker bound)
        delta = max(mp.mpf(y.a) for y in p.pi.ys)-min(mp.mpf(y.b) for y in p.pi.ys)
        if not delta > 0: return None, 'constant sample'
        lo = mp.exp(L0/n)
        bound = lambda a: n*(mp.mpf(3)/5*mp.log(4)+mp.log(a))-mp.mpf(3)/5*a*delta
        knee = 5*n/(3*delta)
    hi = max(mp.mpf(1), knee)
    for _ in range(400):
        if bound(hi) < L0: break
        hi *= 2
    else: return None, 'large-shape cutoff not found'
    # the bound decreases beyond the knee, so L < L0 for all a >= hi
    return lo, hi

def replay_paralogis(r, data, budget, inverse=False):
    res = r['result']; kind = (res.get('outcome') or {}).get('kind'); mask = r['mask']
    x = [float(v) for v in data]
    out = []
    if mask == 3: return [], 'evaluation only'
    if mask == 0 and len(set(x)) == 1 and kind in ('estimate', 'nonexistence'):
        verdict = 'holds' if kind == 'nonexistence' else 'fails'
        return [comparison('constant', 'constant sample with both coefficients free: unbounded likelihood', verdict,
                           'all %d observations equal %s; outcome %s' % (len(x), x[0], kind), rigor='exact', note='exact predicate on the data')], None
    if kind != 'estimate': return [], None
    certified = (res.get('outcome') or {}).get('certified')
    if inverse:
        # the core fits (and certifies) the paralogistic model on the binary64 reciprocals; an uncertified estimate is polished on the original data, so it is replayed on the exact reciprocals
        y = [1.0/v for v in x] if certified else [('inv', v) for v in x]
        shape = number(res['estimates']['shape']); scale_hat = 1/mp.mpf(number(res['estimates']['scale']))
        if mask == 2: scale_hat = mp.mpf(1.0/r["held"]["scale"]) if certified else 1/mp.mpf(r["held"]["scale"])
        held_shape = r['held'].get('shape')
        x = y
    else:
        shape = number(res['estimates']['shape']); scale_hat = mp.mpf(number(res['estimates']['scale']))
        held_shape = r['held'].get('shape')
    tag = '' if certified else '; the fit is flagged uncertified: no certificate claim, this is an independent check'
    ahat = mp.mpf(shape); uhat = mp.log(scale_hat)
    w = mp.mpf('1e-10')
    if mask == 1:
        # shape held, scale free
        pi = ParaLik([iv.log(para_value(v)) for v in x], 'iv')
        sl, sr = mp.log(scale_hat*(1-w)), mp.log(scale_hat*(1+w))
        gl = pi.Lb(iv.mpf(ahat), iv.mpf(-ahat*sl)); gr = pi.Lb(iv.mpf(ahat), iv.mpf(-ahat*sr))
        vl, vr = sign_verdict(gl, -1), sign_verdict(gr, +1)      # smaller scale = larger intercept = L_b < 0 on the left of the root
        verdict = 'fails' if 'fails' in (vl, vr) else 'holds' if vl == vr == 'holds' else 'open'
        out.append(comparison('scale-root', 'log-scale score signed on scale (1 -/+ 1e-10): positive left, negative right (L_b strictly decreasing in b = -a log scale; concave in b, so the root is the unique maximizer)', verdict,
                              'L_b at left %s, at right %s (L_b < 0 left, > 0 right = score positive left, negative right)' % (show(gl), show(gr)),
                              margin='accuracy contract tol 1e-10', note='interval arithmetic at 50 digits on the logs of the stored data'+tag))
        return out, None
    if mask == 2:
        scale = scale_hat if inverse else mp.mpf(r['held']['scale'])
        p = ParaHeld(x, scale); b_hat = iv.mpf(0)
        held = True
    else:
        p = ParaProfile(x, uhat); held = False
    # bracket at the returned shape
    sl, sr = p.score_at(ahat*(1-w)), p.score_at(ahat*(1+w))
    if sl is None or sr is None: return [comparison('bracket', 'shape score signed at the returned shape (1 -/+ 1e-10)', 'open', 'intercept root not enclosed')], None
    vl, vr = sign_verdict(sl, +1), sign_verdict(sr, -1)
    bracket = 'fails' if 'fails' in (vl, vr) else 'holds' if vl == vr == 'holds' else 'open'
    out.append(comparison('bracket', "profile shape score signed on a_hat (1 -/+ 1e-10): positive left, negative right" if not held else "shape score signed on a_hat (1 -/+ 1e-10): positive left, negative right",
                          bracket, 'left %s, right %s' % (show(sl), show(sr)), margin='accuracy contract tol 1e-10', note='interval arithmetic at 50 digits'+tag))
    # value at the returned coefficients (the target of the cover)
    if held: target = lo_(p.pi.L(iv.mpf(ahat), iv.mpf(0)))
    else:
        pi = p.pi; bhat = mp.mpf(0)       # centered at the returned log scale
        gl = pi.Lb(iv.mpf(ahat), iv.mpf(-ahat*mp.log1p(-w))); gr = pi.Lb(iv.mpf(ahat), iv.mpf(-ahat*mp.log1p(w)))
        vl2, vr2 = sign_verdict(gl, -1), sign_verdict(gr, +1)
        sc = 'fails' if 'fails' in (vl2, vr2) else 'holds' if vl2 == vr2 == 'holds' else 'open'
        out.append(comparison('scale-root', 'log-scale score at the returned shape signed on scale (1 -/+ 1e-10): positive left, negative right', sc,
                              'L_b left %s, right %s' % (show(gl), show(gr)), margin='accuracy contract tol 1e-10', note='interval arithmetic at 50 digits'+tag))
        target = lo_(pi.L(iv.mpf(ahat), iv.mpf(bhat)))
        # The profile at the returned shape is attained inside the window (at the
        # continuous log scale), so its lower enclosure is also a valid target. It
        # differs from L at the returned point by the rounding loss of the scale, which
        # is of order 1 on near-constant samples (decisions: logLik at the returned
        # coefficients); elsewhere the two agree to the enclosure width.
        try: target = max(target, lo_(p.value(mp.log(ahat))))
        except ValueError: pass
    L0 = target
    lo, hi = para_cutoffs(p, L0, held)
    if lo is None: return out+[comparison('cover', 'global shape maximum', 'open', hi)], None
    if not hi > lo: return out+[comparison('cover', 'global shape maximum', 'open', 'cutoffs cross')], None
    if bracket != 'holds':
        return out+[comparison('cover', 'global shape maximum', 'open', 'not run: bracket not established')], None
    wexc = mp.mpf('1e-6'); t0 = mp.log(ahat)
    verdict, info = cover(p.value, p.dt, None, mp.log(lo), mp.log(hi), target, [(t0-wexc, t0+wexc)], budget=budget if held else max(budget//2, 200))
    out.append(cover_comparison('cover', 'L(a, .) < %s for every a in [%s, %s] outside the window of relative half width 1e-6 (%s; cutoffs of the dossier tails)' % (
        'L(returned)' if held else 'max(L(returned), P(a_hat))',
        mp.nstr(lo, 6), mp.nstr(hi, 6), 'scale held' if held else 'profile over the intercept'), verdict, info, target,
        note='variable log a%s' % tag, extra='; shape range [%s, %s]' % (mp.nstr(lo, 6), mp.nstr(hi, 6))))
    return out, None

def replay_invparalogis(r, data, budget): return replay_paralogis(r, data, budget, True)

# ------------------------------------------------------------ slash (F)

SLASH_REL = mp.mpf('1e-9'); SLASH_COLLAPSE = mp.mpf('1e-8')
CLAIMS['slash'] = {
    'mu held, sigma free': ('the log-scale score sum g(t_i), g(t) = 1 - 2t/(e^t - 1), t_i = (x_i - mu)^2/(2 sigma^2), decreases strictly from n - 2k to -n (2k < n), so its root is the unique maximizer; '
                            'signed at the returned scale (1 -/+ 1e-10)',
                            'slash/mle.hpp:59-63 (verified sign bracket, bisection, |score| < 1e-3 gate), :299-300 (accuracy tol 1e-10); README "Details" (held-location paragraph) and "Conditional search certificate" (inner scale)'),
    'sigma held, mu free': ('global location cover to the margin 1e-9 (1 + |l|): l(m) < l_hat + 1e-9 (1 + |l_hat|) for every m in [min x, max x] (interval cover; |h\'\'| bounded by max(1, min(z^2/4, 2/z^2 + 1/(e^(z^2/2) - 1))); '
                            'density decreasing in |x - m|, so a maximizer lies in the sample range); location score signed at m_hat -/+ 1e-10 max(|m_hat|, sigma)',
                            'slash/mle.hpp:103-146 (certify_held, margin :125,135), :73-97 (curvature bound); README "Conditional search certificate" (held scale paragraphs)'),
    'both free': ('local accuracy (distance to the stationary point below 1e-10) and global cover of the profile P(m) = max_s l(m, s) to the margin 1e-9 (1 + |l_hat|) (capped 1e-9 n with a half-mass value): the scale root is enclosed by sign checks (n-Lipschitz in log s), '
                  'P\' = l_m at the root (envelope theorem) with |P\'\'| <= A + C^2/B on a cell (A = sum H_i/s_-^2, C = sum (sqrt 2 + sup |z h''|)/s_-, B = sum min(2 t e^(-2t)) at the two ends), '
                  'cells without a positive uniform scale bound use the residual likelihood bound (half-mass supremum n log c - (n-k) log 2 - 2 sum log d); a half-mass value v with B_v >= l_hat - 1e-8 n would be a collapse (returned estimate: B_v < l_hat - 1e-8 n); '
                  'profile score signed at m_hat -/+ 1e-10 max(|m_hat|, sigma_hat)',
                  'slash/mle.hpp:188-272 (certify_free: margins :211-213, :264-266; collapse cutoff boundary + 1e-8 n :213), :314-322 and :406 (boundary), :150-186 (residual bound); README "Conditional search certificate" (free-scale paragraphs)'),
}

def mono(f, t, increasing):
    """Enclosure of a monotone function f (of a thin iv argument) over the interval t from its end values."""
    a, b = iv.mpf(t.a), iv.mpf(t.b)
    return iv.mpf([f(a).a, f(b).b]) if increasing else iv.mpf([f(b).a, f(a).b])

def _g(x):        # 1 - 2x/(e^x - 1), increasing, -1 at 0
    return iv.mpf(-1) if x.a == 0 and x.b == 0 else 1-2*x/iv.expm1(x)
def _kappa(x):    # 1/x - 1/(e^x - 1) = 1/2 - x/12 + x^3/720 - ..., decreasing, 1/2 at 0
    if x.b < mp.mpf('1e-3'):
        return iv.mpf(1)/2-x/12+x**3/720+iv.mpf([-x.b**5/30240, 0])
    return 1/x-1/iv.expm1(x)
def _q(x):        # (1 - e^(-x))/x, decreasing, 1 at 0
    return iv.mpf(1) if x.a == 0 and x.b == 0 else -iv.expm1(-x)/x
def slash_g(t): return mono(_g, t, True)
def slash_kappa(t): return mono(_kappa, t, False)
def slash_q(t): return mono(_q, t, False)

class SlashLik:
    """Slash log likelihood l(m, s) = sum [-log s - log(2 pi)/2 + log(q(t_i)/2)], q(t) = (1 - e^-t)/t, t_i = (x_i - m)^2/(2 s^2); residual vectors are interval lists."""
    def __init__(self, x):
        self.x = [float(v) for v in x]; self.n = len(self.x)
        self.half = iv.log(2*iv.pi)/2
    def resid(self, m):
        mi = iv.mpf(m); return [iv.mpf(v)-mi for v in self.x]
    def cell(self, a, b):
        """(near, far) lower ends of the distances to the cell [a, b] and upper ends of the largest distances, as thin intervals."""
        near, far = [], []
        for v in self.x:
            fv = mp.mpf(v)
            if a <= fv <= b: nr = iv.mpf(0)
            elif fv < a: nr = iv.mpf(a)-iv.mpf(v)
            else: nr = iv.mpf(v)-iv.mpf(b)
            fr = iv.mpf(max(abs(iv.mpf(v)-iv.mpf(a)).b, abs(iv.mpf(v)-iv.mpf(b)).b))
            near.append(iv.mpf(nr.a)); far.append(fr)
        return near, far
    @staticmethod
    def zeros(R): return sum(1 for r in R if r.a == 0 and r.b == 0)
    def tvals(self, R, s): return [(r*r)/(2*s*s) for r in R]
    def gsum(self, R, s):
        total = iv.mpf(0)
        for t in self.tvals(R, s): total += slash_g(t)
        return total
    def lm(self, R, s):
        """l_m = sum r kappa(t)/s^2 (slope in the location)"""
        total = iv.mpf(0)
        for r, t in zip(R, self.tvals(R, s)): total += r*slash_kappa(t)/(s*s)
        return total
    def loglik(self, R, s):
        total = iv.mpf(0)
        for t in self.tvals(R, s): total += -iv.log(s)-self.half+iv.log(slash_q(t)/2)
        return total
    def collapse(self, R):
        """half-mass supremum of the residual log likelihood at 2k = n: -n/2 log(2 pi) - (n-k) log 2 - 2 sum log d_i (d_i > 0)"""
        k = self.zeros(R); total = -self.n*self.half-(self.n-k)*iv.log(2)
        for r in R:
            if not (r.a == 0 and r.b == 0): total -= 2*iv.log(iv.mpf(abs(r).a))
        return total

def slash_root_mp(absr, tol=None):
    """Approximate root (plain mpmath, safeguarded Newton in log s) of sum g(t_i) at residuals absr, to `tol` in log s (default 1e-30); None when 2k >= n."""
    n = len(absr); k = sum(1 for r in absr if r == 0)
    if 2*k >= n: return None
    tol = mp.mpf(10)**-30 if tol is None else tol
    pos = [r for r in absr if r > 0]
    lo = mp.log(min(pos))-mp.log(mp.sqrt(2*4*n/(n-2*k)))-1; hi = mp.log(max(pos)*mp.sqrt(2))+1
    def score(v, deriv=False):
        s2 = mp.exp(2*v)*2; total = mp.mpf(0); slope = mp.mpf(0)
        for r in absr:
            t = r*r/s2
            if t == 0: total -= 1; continue
            e = mp.expm1(t)
            total += 1-2*t/e
            if deriv: slope -= 2*t*(1 if t < mp.mpf('1e-10') else 2*(t*(e+1)-e)/(e*e))
        return (total, slope) if deriv else total
    for _ in range(80):
        if score(lo) > 0: break
        lo -= 4
    for _ in range(80):
        if score(hi) < 0: break
        hi += 4
    v = (lo+hi)/2
    for _ in range(300):
        sv, dv = score(v, True)
        if sv > 0: lo = v
        else: hi = v
        if hi-lo < tol: break
        nv = v-sv/dv if dv < 0 else (lo+hi)/2
        if not lo < nv < hi: nv = (lo+hi)/2
        done = abs(nv-v) < tol
        v = nv
        if done: break
    return mp.exp(v)

def slash_bracket_s(p, R, kappa=mp.mpf('1e-22')):
    """(s_lo, s_hi) mpf with the score certain positive at s_lo and negative at s_hi (so the unique root lies between); None if 2k >= n or not found."""
    absr = [mp.mpf(r.a)/2+mp.mpf(r.b)/2 if not (r.a == 0 and r.b == 0) else mp.mpf(0) for r in R]
    absr = [abs(v) for v in absr]
    s0 = slash_root_mp(absr)
    if s0 is None: return None
    for _ in range(60):
        lo, hi = s0*(1-kappa), s0*(1+kappa)
        if lo_(p.gsum(R, iv.mpf(lo))) > 0 and hi_(p.gsum(R, iv.mpf(hi))) < 0: return lo, hi
        kappa *= 4
    return None

def slash_value(p, m):
    """Enclosure of the profile P(m) = max_s l(m, s) at an exact location m (free scale)."""
    R = p.resid(m); k = p.zeros(R); n = p.n
    if 2*k > n: return iv.mpf([-mp.inf, mp.inf])
    if 2*k == n: return iv.mpf([-mp.inf, hi_(p.collapse(R))])        # strictly below the half-mass supremum at every s (README)
    sb = slash_bracket_s(p, R)
    if sb is None: raise ValueError('scale root not enclosed')
    lo, hi = sb; sm = (lo+hi)/2
    v = p.loglik(R, iv.mpf(sm))
    return iv.mpf([lo_(v), hi_(v)+n*mp.log(hi/lo)/2])       # |score| <= n: P - l(s_mid) <= n |log s* - log s_mid|

def slash_hbound(z_near, z_far):
    """H = max(1, min(z_far^2/4, 2/z_near^2 + 1/(e^(z_near^2/2) - 1))) as an upper bound of |h''| (thin intervals; z_near may be 0)."""
    top = z_far*z_far/4
    if z_near.a > 0:
        tail = 2/(z_near*z_near)+1/iv.expm1(z_near*z_near/2)
        top = iv.mpf(min(top.b, tail.b))
    return iv.mpf(max(mp.mpf(1), top.b))

def slash_zh(z_near, z_far):
    """Upper bound of sup |z h''(z)| over z in [z_near, z_far] (thin intervals). Mine, sharper than the dossier's z_far H for the cross derivative:
    h'' = 1/t - 2 t e^t/(e^t - 1)^2 + 1/(e^t - 1) with t = z^2/2, so h'' >= 0 once 2 sinh(t/2) >= sqrt(2) t (t >= 3.4, z >= 2.6), where z h'' <= u(z) = 2/z + z/(e^t - 1)
    (decreasing for z >= 1); below z = 2.6 the dossier's -1 <= h'' <= z^2/4 gives |z h''| <= 2.6 max(1, 2.6^2/4) < 5."""
    if z_near.a >= mp.mpf('2.6'):
        u = 2/z_near+z_near/iv.expm1(z_near*z_near/2)
        return iv.mpf(u.b)
    top = z_far*max(mp.mpf(1), z_far.b**2/4)
    return iv.mpf(min(top.b, mp.mpf(5)))

class SlashProfile:
    """Joint (free scale): see CLAIMS['slash']['both free']."""
    def __init__(self, x):
        self.p = SlashLik(x); self.n = self.p.n; self.cache = {}
    def value(self, m):
        if m not in self.cache: self.cache[m] = slash_value(self.p, m)
        return self.cache[m]
    def score_at(self, m):
        R = self.p.resid(m); sb = slash_bracket_s(self.p, R)
        if sb is None: return None
        return self.p.lm(R, iv.mpf([sb[0], sb[1]]))
    def bounds(self, a, b):
        """(s_minus, s_plus, near, far) or None: uniform scale bounds over the cell [a, b]."""
        near, far = self.p.cell(a, b)
        s0 = slash_root_mp([mp.mpf(r.a) for r in near], mp.mpf('1e-4')); s1 = slash_root_mp([mp.mpf(r.b) for r in far], mp.mpf('1e-4'))
        if s0 is None or s1 is None: return None
        smin = s0*(1-mp.mpf('1e-3')); smax = s1*(1+mp.mpf('1e-3'))
        for _ in range(60):
            if lo_(self.p.gsum(near, iv.mpf(smin))) > 0: break
            smin /= 2
        else: return None
        for _ in range(60):
            if hi_(self.p.gsum(far, iv.mpf(smax))) < 0: break
            smax *= 2
        else: return None
        return smin, smax, near, far
    def upper(self, a, b):
        near, far = self.p.cell(a, b); n = self.n; k = self.p.zeros(near)
        if 2*k > n: return None
        if 2*k == n: return self.p.collapse(near)
        sb = slash_bracket_s(self.p, near)
        if sb is None: return None
        lo, hi = sb; v = self.p.loglik(near, iv.mpf((lo+hi)/2))
        return iv.mpf([lo_(v), hi_(v)+n*mp.log(hi/lo)/2])
    def deriv(self, a, b):
        full = iv.mpf([-mp.inf, mp.inf])
        bd = self.bounds(a, b)
        if bd is None: return full
        smin, smax, near, far = bd
        c = (a+b)/2; pc = self.score_at(c)
        if pc is None: return full
        A = C = Bs = iv.mpf(0)
        for nr, fr in zip(near, far):
            zl, zh = nr/iv.mpf(smax), fr/iv.mpf(smin)
            H = slash_hbound(zl, zh)
            A += H/(smin*smin); C += (iv.sqrt(2)+slash_zh(zl, zh))/smin
            tl, th = zl*zl/2, zh*zh/2
            e1, e2 = 2*tl*iv.exp(-2*tl), 2*th*iv.exp(-2*th)
            Bs += iv.mpf(min(e1.a, e2.a))
        if not lo_(Bs) > 0: return full
        K = A+C*C/Bs; r = iv.mpf((b-a)/2)
        return pc+iv.mpf([-1, 1])*K*r

class SlashHeld:
    """sigma held: location likelihood and |h''| bound cover."""
    def __init__(self, x, sigma):
        self.p = SlashLik(x); self.n = self.p.n; self.s = iv.mpf(sigma)
    def value(self, m): return self.p.loglik(self.p.resid(m), self.s)
    def score_at(self, m): return self.p.lm(self.p.resid(m), self.s)
    def upper(self, a, b): return self.p.loglik(self.p.cell(a, b)[0], self.s)
    def deriv(self, a, b):
        near, far = self.p.cell(a, b); c = (a+b)/2
        A = iv.mpf(0)
        for nr, fr in zip(near, far): A += slash_hbound(nr/self.s, fr/self.s)/(self.s*self.s)
        return self.score_at(c)+iv.mpf([-1, 1])*A*iv.mpf((b-a)/2)

def slash_stationary(p, mu, sigma):
    """Distance from (mu, sigma) to the nearby stationary point of the 2-D log likelihood, as (|dmu|/max(|mu|, sigma), |d log sigma|, negative definite, converged):
    full Newton iteration at 50 digits in q = mu/c, v = log sigma (c = max(|mu|, sigma)); gradient from the interval scores (midpoints), Hessian by central differences."""
    mid = lambda z: (mp.mpf(z.a)+mp.mpf(z.b))/2
    c = max(abs(mu), sigma)
    def grad(q, v):
        R = p.resid(q*c); s = iv.mpf(mp.exp(v))
        return mid(p.lm(R, s))*c, mid(p.gsum(R, s))
    q0 = mu/c; v0 = mp.log(sigma); q, v = q0, v0
    hq = mp.mpf(10)**-8*min(mp.mpf(1), sigma/c); hv = mp.mpf(10)**-8
    converged = False; definite = False
    for _ in range(60):
        g = grad(q, v)
        gq1, gq0 = grad(q+hq, v), grad(q-hq, v); gv1, gv0 = grad(q, v+hv), grad(q, v-hv)
        H = mp.matrix([[(gq1[0]-gq0[0])/(2*hq), (gv1[0]-gv0[0])/(2*hv)], [(gq1[1]-gq0[1])/(2*hq), (gv1[1]-gv0[1])/(2*hv)]])
        definite = H[0, 0] < 0 and H[0, 0]*H[1, 1]-H[0, 1]*H[1, 0] > 0
        if not definite: break
        step = mp.lu_solve(H, mp.matrix([-g[0], -g[1]]))
        q += step[0]; v += step[1]
        if max(abs(step[0]), abs(step[1])) < mp.mpf(10)**-25: converged = True; break
    return abs(q-q0), abs(v-v0), definite, converged

def replay_slash(r, data, budget):
    try: return replay_slash_run(r, data, budget)
    except (ZeroDivisionError, ValueError, OverflowError) as error:
        return [comparison('cover', 'evaluation raised', 'open', 'slash evaluation: %s: %s' % (type(error).__name__, error), rigor='interval')], None

def replay_slash_run(r, data, budget):
    res = r['result']; kind = (res.get('outcome') or {}).get('kind'); mask = r['mask']
    if kind != 'estimate' or mask == 3: return [], None
    x = [float(v) for v in data]; n = len(x)
    mu = number(res['estimates']['mu']); sigma = number(res['estimates']['sigma'])
    if not (sigma > 0 and math.isfinite(sigma) and math.isfinite(mu)): return [], 'returned coefficients outside the model'
    w = mp.mpf('1e-10'); out = []
    counts = collections.Counter(x)
    if mask == 1:
        p = SlashLik(x); R = p.resid(mu)
        k = p.zeros(R)
        if 2*k >= n: return [], 'half or more at mu (nonexistence; faces module)'
        gl, gr = p.gsum(R, iv.mpf(sigma)*(1-iv.mpf(w))), p.gsum(R, iv.mpf(sigma)*(1+iv.mpf(w)))
        vl, vr = sign_verdict(gl, +1), sign_verdict(gr, -1)
        verdict = 'fails' if 'fails' in (vl, vr) else 'holds' if vl == vr == 'holds' else 'open'
        out.append(comparison('scale-bracket', 'log-scale score signed on sigma_hat (1 -/+ 1e-10): positive left, negative right (strictly decreasing from n - 2k to -n: unique root)', verdict,
                              'left %s, right %s' % (show(gl), show(gr)), margin='accuracy contract tol 1e-10; gate |score| < 1e-3', note='interval arithmetic at 50 digits'))
        return out, None
    scale_w = max(abs(mp.mpf(mu)), mp.mpf(sigma))
    if mask == 2:
        h = SlashHeld(x, sigma)
        sl, sr = h.score_at(mp.mpf(mu)-w*scale_w), h.score_at(mp.mpf(mu)+w*scale_w)
        vl, vr = sign_verdict(sl, +1), sign_verdict(sr, -1)
        bracket = 'fails' if 'fails' in (vl, vr) else 'holds' if vl == vr == 'holds' else 'open'
        out.append(comparison('bracket', 'location score signed on mu_hat -/+ 1e-10 max(|mu_hat|, sigma): positive left, negative right', bracket,
                              'left %s, right %s' % (show(sl), show(sr)), margin='accuracy contract tol 1e-10', note='interval arithmetic at 50 digits; half width %s' % mp.nstr(w*scale_w, 4)))
        lhat = lo_(h.value(mp.mpf(mu)))
        target = lhat+SLASH_REL*(1+abs(lhat))
        verdict, info = cover(h.value, h.deriv, h.upper, mp.mpf(min(x)), mp.mpf(max(x)), target, [], budget=budget*5)
        out.append(cover_comparison('cover', 'l(m, sigma) < l_hat + 1e-9 (1 + |l_hat|) for every m in [min x, max x] (density decreasing in |x - m|: the sample range contains a maximizer)', verdict, info, target,
                                    note='total log likelihood; curvature bound |h\'\'| <= max(1, min(z^2/4, 2/z^2 + 1/(e^(z^2/2)-1))) of the dossier'))
        return out, None
    # both free
    pr = SlashProfile(x); p = pr.p
    R = p.resid(mu)
    if 2*p.zeros(R) >= n: return [], 'half or more at mu_hat'
    sl, sr = pr.score_at(mp.mpf(mu)-w*scale_w), pr.score_at(mp.mpf(mu)+w*scale_w)
    if sl is None or sr is None: return [comparison('bracket', 'profile location score signed at mu_hat -/+ 1e-10 max(|mu_hat|, sigma_hat)', 'open', 'scale root not enclosed')], None
    vl, vr = sign_verdict(sl, +1), sign_verdict(sr, -1)
    bracket = 'fails' if 'fails' in (vl, vr) else 'holds' if vl == vr == 'holds' else 'open'
    out.append(comparison('bracket', 'profile location score signed on mu_hat -/+ 1e-10 max(|mu_hat|, sigma_hat): positive left, negative right', bracket,
                          'left %s, right %s' % (show(sl), show(sr)), margin='accuracy contract tol 1e-10', note='interval arithmetic at 50 digits; half width %s' % mp.nstr(w*scale_w, 4)))
    d_mu, d_v, definite, converged = slash_stationary(p, mp.mpf(mu), mp.mpf(sigma))
    verdict = 'open' if not (definite and converged) else 'holds' if d_mu <= w and d_v <= w else 'fails'
    out.append(comparison('accuracy', 'the returned coefficients are within tol = 1e-10 of the nearby stationary point (Hessian negative definite): |dmu|/max(|mu|, sigma) and |d log sigma| below 1e-10', verdict,
                          '|dmu|/max(|mu|, sigma) = %s, |d log sigma| = %s%s' % (mp.nstr(d_mu, 4), mp.nstr(d_v, 4), '' if definite and converged else '; Newton did not converge to a negative definite point'),
                          margin='accuracy contract tol 1e-10', rigor='numerical',
                          note='full Newton iteration at 50 digits from the returned coefficients (gradient from the interval scores, Hessian by central differences); the distance travelled is the coefficient error. A stationarity check, not an enclosure'))
    lhat = lo_(p.loglik(R, iv.mpf(sigma)))
    # half-mass values: boundary B_v
    boundary = None; bv = None
    for v, c in counts.items():
        if 2*c == n:
            Bv = hi_(p.collapse(p.resid(v)))
            if boundary is None or Bv > boundary: boundary, bv = Bv, v
    if boundary is not None:
        ok = boundary < lhat-SLASH_COLLAPSE*n
        out.append(comparison('collapse', 'the half-mass supremum B_v is below l_hat - 1e-8 n (otherwise the core returns the collapse limit)', 'holds' if ok else 'fails',
                              'largest B_v %s at v = %s, l_hat - 1e-8 n = %s' % (mp.nstr(boundary, 15), bv, mp.nstr(lhat-SLASH_COLLAPSE*n, 15)), margin='1e-8 n',
                              note='B_v = -n/2 log(2 pi) - (n/2) log 2 - 2 sum log |x_i - v|; a returned estimate within 1e-8 n of the collapse would contradict the decision rule'))
    margin = SLASH_REL*(1+abs(lhat))
    if boundary is not None: margin = min(margin, SLASH_REL*n)
    target = lhat+margin
    if bracket != 'holds':
        return out+[comparison('cover', 'global profile maximum', 'open', 'not run: bracket not established')], None
    verdict, info = cover(pr.value, pr.deriv, pr.upper, mp.mpf(min(x)), mp.mpf(max(x)), target, [], budget=budget*2)
    out.append(cover_comparison('cover', 'P(m) < l_hat + margin for every m in [min x, max x], margin = 1e-9 (1 + |l_hat|)%s' % (' (capped 1e-9 n: half-mass value present)' if boundary is not None else ''),
                                verdict, info, target, note='profile over the scale (root enclosed by interval sign checks); |P\'\'| <= A + C^2/B per cell; residual bound where no uniform scale bound exists'))
    return out, None

# ------------------------------------------------------------ shifted Gompertz (F)

def sgomp_cover_comparison(name, claim, verdict, info, target):
    best, location = info['best']
    detail = f'target {mp.nstr(target, 20)}. '
    if verdict == 'fails':
        detail += f'profile witness {show(info["value"], 20)} at rate {mp.nstr(info["point"], 18)}'
    else:
        detail += f'largest lower bound {mp.nstr(best, 20)} at rate {mp.nstr(location, 18)}'
        if verdict == 'open':
            detail += '. ' + info['reason']
    return comparison(name, claim, verdict, detail,
                      note=f'{info["evaluations"]} evaluations. Rate in max(x) units')


def replay_sgomp(r, data, budget):
    from cert_replay_sgomp import Profile

    result = r['result']
    outcome = result.get('outcome') or {}
    if r['mask'] or outcome.get('kind') != 'estimate' or not outcome.get('certified'):
        return [], None

    claim = 'joint rate profile within eight arithmetic margins of the returned likelihood'
    comps = []
    phase = 'rate-cover'
    try:
        p = Profile([float(x) for x in data])
        estimates = result['estimates']
        rate = iv.mpf(number(estimates['b'])) * max(data)
        eta = iv.mpf(number(estimates['eta']))
        incumbent = p.likelihood(rate, eta)
        margin = 4096 * iv.mpf(2)**-52 * (p.n + abs(incumbent))
        threshold = incumbent + margin
        target = lo_(threshold)

        low = min(mp.mpf('0.5'), mp.exp(target / p.n - 2))
        high = max(2 * p.n / lo_(p.gap), 2 * hi_(rate))
        for _ in range(256):
            bound = p.n * iv.log(iv.mpf(high) * p.n) - iv.mpf(high) * p.gap
            if bound.b < target and (p.n / iv.mpf(high) - p.gap).b < 0:
                break
            high *= 2
        else:
            raise ValueError('could not close large-rate envelope')
        if (p.n * (iv.log(iv.mpf(low)) + 1)).b >= target:
            raise ValueError('could not close small-rate envelope')

        verdict, info = cover(p.value, p.derivative, p.upper, low, high, target,
                              budget=budget)
        if verdict == 'fails' and lo_(info['value']) <= hi_(threshold):
            verdict = 'open'
            info['reason'] = 'witness overlaps likelihood threshold ' + show(threshold)
        comps.append(sgomp_cover_comparison(phase, claim, verdict, info, target))
        if verdict == 'fails':
            return comps, None

        phase = 'rate-enclosure'
        width = iv.mpf('1e-7')
        wl = hi_(rate * (1-width))
        wr = lo_(rate * (1+width))
        enclosure_claim = 'all global maximizers are within the rate and shape windows'
        if not low < wl < wr < high:
            comps.append(comparison('rate-enclosure', enclosure_claim, 'open',
                                    'compact rate bounds do not contain the returned window'))
            return comps, None

        # The tail bounds must also exclude competitors at the unpadded value.
        witness = lo_(incumbent)
        small_bound = p.n * (iv.log(iv.mpf(low)) + 1)
        large_bound = p.n * iv.log(iv.mpf(high)*p.n) - iv.mpf(high)*p.gap
        if max(small_bound.b, large_bound.b) >= witness:
            comps.append(comparison('rate-enclosure', enclosure_claim, 'open',
                                    'tail bound overlaps the unpadded likelihood'))
            return comps, None
        verdict, info = cover(p.value, p.derivative, p.upper, low, high,
                              witness, [(wl, wr)], budget=budget)
        if verdict == 'fails':
            inside_bound = p.upper(wl, wr)
            if inside_bound is None or info['value'].a <= inside_bound.b:
                verdict = 'open'
                info['reason'] = 'competitor above returned likelihood overlaps the rate-window upper bound'
        comps.append(sgomp_cover_comparison(phase, enclosure_claim, verdict, info, witness))
        if verdict != 'holds':
            return comps, None

        phase = 'shape-enclosure'
        shape = p.shape_range(*p.root_window(wl, wr))
        radius = width * max(iv.mpf(1), eta)
        shape_lo, shape_hi = eta-radius, eta+radius
        if shape.a > shape_lo.b and shape.b < shape_hi.a:
            verdict = 'holds'
        elif shape.b < shape_lo.a or shape.a > shape_hi.b:
            verdict = 'fails'
        else:
            verdict = 'open'
        comps.append(comparison('shape-enclosure', enclosure_claim, verdict,
                                f'conditional shape {show(shape)}. Window {show(shape_lo)}, {show(shape_hi)}',
                                margin='1e-7 max(1, eta)', note='monotone endpoint shape enclosure'))
        return comps, None
    except (ValueError, ZeroDivisionError, OverflowError) as error:
        return comps+[comparison(phase, claim, 'open', str(error))], None
    finally:
        Profile.shape.cache_clear()


# ------------------------------------------------------------ transformed gamma (E, F)

def replay_trgamma(r, data, budget):
    from cert_replay_trgamma import Profile

    result = r['result']
    kind = (result.get('outcome') or {}).get('kind')
    if r['mask'] not in (0, 4) or kind not in ('estimate', 'limit'):
        return [], None
    if kind == 'estimate' and not result['outcome'].get('certified'):
        return [], 'uncertified estimate has no power-cover claim'
    if kind == 'limit' and r['mask']:
        return [], 'held-scale limit has no closed global certificate'
    logs = [mp.log(mp.mpf(float(v))) for v in data if float(v) > 0]
    if len(logs) != len(data) or len(set(logs)) < 2:
        return [], 'degenerate sample or outside support'
    reflected = r['family'] == 'invtrgamma'
    if reflected:
        logs = [-x for x in logs]
    logscale = mp.log(mp.mpf(r['held']['scale'])) if r['mask'] else None
    if reflected and logscale is not None:
        logscale = -logscale
    claim = 'convex-conjugate cover of the standardized power profile'
    comps = []
    try:
        p = Profile(logs, logscale)
        basin = None
        eps = mp.mpf(2)**-52
        if kind == 'estimate':
            coeff = result['estimates']
            shape, power, scale = (mp.mpf(number(coeff[k])) for k in ('shape1', 'shape2', 'scale'))
            t = power*p.unit
            estimate_logscale = (-1 if reflected else 1)*mp.log(scale)
            value = p.returned(t, shape, estimate_logscale)
            radius = min(mp.mpf('2e-4'), max(mp.mpf('1e-6'), t*t/100)) if p.free_scale else mp.mpf('1e-4')
            for _ in range(12):
                basin = t*(1-radius), t*(1+radius)
                sl, sr = (p.point(b)[1] for b in basin)
                upper_curvature = p.curvature(*basin)
                if upper_curvature < 0 or not sl > 0 > sr:
                    break
                radius /= 2
            if not (sl > 0 > sr and upper_curvature < 0):
                comps.append(comparison('power-basin', 'signed and concave root neighborhood', 'open',
                                        'scores %s, %s, curvature bound %s' % tuple(mp.nstr(v, 18) for v in (sl, sr, upper_curvature)),
                                        rigor='numerical'))
                return comps, None
            comps.append(comparison('power-basin', 'signed and concave root neighborhood', 'holds',
                                    'scores %s, %s, curvature bound %s' % tuple(mp.nstr(v, 18) for v in (sl, sr, upper_curvature)),
                                    rigor='numerical'))
            target = max(value, p.point(t)[0])
            if mp.isfinite(p.face):
                gap = target-p.face
                upper = p.upper(*basin)-p.face
                margin = mp.mpf('1e-8')
                guard = mp.eps**mp.mpf('0.4')*(1+abs(target))
                verdict = 'holds' if gap > margin+guard else 'fails' if upper < margin-guard else 'open'
                comps.append(comparison('power-face-gap', 'interior optimum exceeds both available faces by 1e-8 per observation',
                                        verdict, f'gap from {mp.nstr(gap, 20)} to {mp.nstr(upper, 20)}',
                                        margin='1e-8', rigor='numerical'))
                if verdict == 'fails':
                    return comps, None
        else:
            model = result.get('model')
            if model not in ('lnorm', 'power', 'pareto'):
                raise ValueError(f'unknown limit model {model!r}')
            chosen = mp.mpf(0) if model == 'lnorm' else p.constant-mp.mpf('0.5')-mp.log(p.maximum)
            if chosen < p.face:
                gap = p.face-chosen
                verdict = 'fails' if gap > mp.eps**mp.mpf('0.4') else 'open'
                return [comparison('power-face-gap', 'chosen face maximizes the two face likelihoods',
                                   verdict, f'chosen {mp.nstr(chosen, 20)}, best {mp.nstr(p.face, 20)}',
                                   rigor='numerical')], None
            target = chosen + 64*eps*(1+2*abs(chosen))
        lo, hi = p.tails(target)
        if basin and not lo < basin[0] < basin[1] < hi:
            raise ValueError('root neighborhood extends beyond compact domain')
        verdict, detail = p.cover(lo, hi, target, budget, basin)
        if verdict == 'fails' and basin:
            # A profile witness above the returned point can still be below the
            # local maximizer. Bound that maximizer by the secant on its basin.
            verdict2, detail2 = p.cover(lo, hi, p.upper(*basin), budget, basin)
            if verdict2 == 'fails':
                detail = detail2
            else:
                verdict = 'open'
                detail += '; witness lies within the local basin value bound'
        comps.append(comparison('power-cover', claim, verdict,
                                f'target {mp.nstr(target, 20)} per observation. {detail}',
                                rigor='numerical', note='mpmath inverse digamma and secant tangents'))
        return comps, None
    except (ValueError, ZeroDivisionError, OverflowError) as error:
        return comps+[comparison('power-cover', claim, 'open', str(error), rigor='numerical')], None
    finally:
        Profile.point.cache_clear()


# ------------------------------------------------------------ Huber epsilon profile (E, F)

def replay_huber(r, data, budget):
    from cert_replay_huber import Profile

    result = r['result']
    outcome = result.get('outcome') or {}
    kind = outcome.get('kind')
    if r['mask'] & 4 or kind not in ('estimate', 'limit') or not outcome.get('certified'):
        return [], None
    claim = 'epsilon profile bounded by nuisance supporting planes and the log normalizer chord error'
    held = r['held']
    mu = mp.mpf(held['mu']) if 'mu' in held else None
    sigma = mp.mpf(held['sigma']) if 'sigma' in held else None
    comps = []
    try:
        profile = Profile([mp.mpf(float(v)) for v in data], mu, sigma)
        if kind == 'estimate':
            coeff = result['estimates']
            m, s, c = (mp.mpf(number(coeff[k])) for k in ('mu', 'sigma', 'epsilon'))
            incumbent = profile.likelihood((m-profile.origin)/profile.unit, s/profile.unit, c)
            other = max(profile.normal, profile.laplace)
            target = incumbent + mp.mpf('1e-8')*profile.n
        else:
            model = result.get('model')
            if model not in ('norm', 'laplace'):
                raise ValueError(f'unknown limit model {model!r}')
            normal = model == 'norm'
            if not normal and sigma is not None:
                return [comparison('epsilon-face', 'a Laplace face needs free scale',
                                   'fails', f'held sigma {sigma}', rigor='exact')], None
            incumbent = profile.normal if normal else profile.laplace
            other = profile.laplace if normal else profile.normal
            target = incumbent-1024*mp.mpf(2)**-52*(1+abs(incumbent)+profile.n)
        pad = 1024*mp.mpf(2)**-52*(1+abs(incumbent)+profile.n)
        if kind == 'limit' and model == 'norm' and all(x == profile.normal_mu for x in profile.x):
            return [comparison('epsilon-cover', 'all Gaussian residuals vanish with scale held',
                               'holds', 'normal tail starts at epsilon = 0', rigor='exact')], None
        gap = incumbent-other
        if mp.isfinite(other):
            guard = mp.eps**mp.mpf('0.4')*(1+abs(incumbent))
            face_pad = pad + 128*mp.mpf(2)**-52*(1+abs(other)+profile.n)
            verdict = 'holds' if gap > face_pad+guard else 'fails' if gap < face_pad-guard else 'open'
            comps.append(comparison('epsilon-face', 'winner above the competing faces by the arithmetic pad',
                                    verdict, f'gap {mp.nstr(gap, 20)}, pad {mp.nstr(face_pad, 20)}',
                                    rigor='numerical'))
            if verdict == 'fails':
                return comps, None
        verdict, detail = profile.cover(incumbent, target, budget, result.get('model') == 'laplace')
        comps.append(comparison('epsilon-cover', claim, verdict, f'target {mp.nstr(target, 20)}. {detail}',
                                rigor='numerical', note='mpmath nuisance solve and supporting plane'))
        return comps, None
    except (ValueError, ZeroDivisionError, OverflowError) as error:
        return comps+[comparison('epsilon-cover', claim, 'open', str(error), rigor='numerical')], None
    finally:
        Profile.point.cache_clear()


# ------------------------------------------------------------ registry

def both(first, second):
    def run(r, data, budget):
        a, why_a = first(r, data, budget); b, why_b = second(r, data, budget)
        return (a or [])+(b or []), why_a or why_b
    return run

REPLAYS = {
    'pareto2': both(replay_pareto2, replay_lomax_limit_transfer),
    'invpareto': both(replay_invpareto, replay_lomax_limit_transfer),
    'cauchy': replay_cauchy_scale,
    'foldnorm': replay_foldnorm, 'fatigue': replay_fatigue, 'skellam': replay_skellam,
    'trgamma': replay_trgamma, 'invtrgamma': replay_trgamma, 'huber': replay_huber,
    'sgomp': replay_sgomp,
    'betapr': replay_betapr_unit,
    'burr': replay_burr_power,
    'dgamma': both(replay_dgamma, replay_dgamma_joint),
    'ht': both(replay_ht, replay_ht_face),
    'slash': replay_slash,
    'paralogis': replay_paralogis, 'invparalogis': replay_invparalogis,
    'gpd': both(both(replay_gpd, replay_gpd_coefficient), replay_gpd_xi_held),
}
