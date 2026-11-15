"""Certificate replay, mechanisms B and C (scalar roots and represented likelihood).

Companion of cert-replay.py (design: dev/design/certificate-replay.md; report:
dev/research/evaluations/2026-11-14-replay-roots-report.md). Registered into
cert-replay.py's REPLAYS; every function has the signature
replay(record, data, budget) -> (comparisons, why_none) of the prototype.

Code read at master 2fd54d96 (package code identical to 7e3c9779, the commit of
the stored baselines). Claimed widths are listed in CLAIMS below with the
source line they were read from; a change to a certificate must update the
entry (and the width in the replay function that uses it) in the same commit.
Controls: the snapshot calls every fitter with the binding's default controls
(TOL below, with the binding line).

Comparisons (names are the check names, cert-replay:<name>):

  root      C. The signs of the score the certificate brackets, at the returned
            coefficient times (1 -/+ w), w the claimed half width plus the
            rounding of the returned double. holds / fails / open as in the
            prototype. Interval arithmetic (mpmath iv, outward rounded) where
            the score is elementary, otherwise 50-digit mpmath with a second
            evaluation at 80 digits: the enclosure is the hull of the two
            values widened by ten times their difference ("numerical").
            Where the certificate rests on restored() the root check is
            reported with verdict 'info' (not judged).
  restore   B. core/transformed_data.hpp restored(): the supremum minus the
            likelihood at the rounded public coefficients, against
            tol*max(1,|ideal|) + 128 eps (1 + |ideal| + |actual|). The
            supremum is the 50-digit maximizer; the likelihood is the dossier
            log_density summed at the returned coefficients.
  moment    modifier routes: the conditional-mean equation at the returned
            coefficient times (1 -/+ w).

Every root comparison records, when it can be computed, the offset of the
returned coefficient from the 50-digit root ('hp offset'); that value is
numerical and informational.
"""
import collections, contextlib, math
from fractions import Fraction
import mpmath as mp
from strict_common import number

iv = mp.iv
EPS = 2.0**-52
HALF = 2.0**-53
DPS_STEPS = (50, 100, 200, 400)
CLAIMS = {}      # family -> {subset: (width text, source)}; filled below and written to the records


# ------------------------------------------------------------ arithmetic

class IVOps:
    """Outward-rounded interval arithmetic."""
    rigor = 'interval'
    mpf = staticmethod(iv.mpf)
    exp = staticmethod(iv.exp); log = staticmethod(iv.log); log1p = staticmethod(iv.log1p)
    expm1 = staticmethod(iv.expm1); sqrt = staticmethod(iv.sqrt); loggamma = staticmethod(iv.loggamma)
    @staticmethod
    def exact(v): return iv.mpf(v)

class MPOps:
    """Ordinary mpmath at the working precision (numerical)."""
    rigor = 'numerical'
    mpf = staticmethod(mp.mpf)
    exp = staticmethod(mp.exp); log = staticmethod(mp.log); log1p = staticmethod(mp.log1p)
    expm1 = staticmethod(mp.expm1); sqrt = staticmethod(mp.sqrt); loggamma = staticmethod(mp.loggamma)
    digamma = staticmethod(mp.digamma)
    @staticmethod
    def psi1(z): return mp.psi(1, z)
    @staticmethod
    def exact(v): return mp.mpf(v)

@contextlib.contextmanager
def precision(d):
    saved = (iv.dps, mp.mp.dps)
    iv.dps = d; mp.mp.dps = d
    try: yield
    finally: iv.dps, mp.mp.dps = saved

def hull(a, b):
    lo, hi = min(a, b), max(a, b)
    pad = 10*abs(a-b)+abs(b)*mp.mpf(10)**(-mp.mp.dps+10)
    return iv.mpf([lo-pad, hi+pad])

def enclose(make, rigor, d):
    """make(A) -> tuple of A numbers. Interval: A = IVOps. Numerical: values at d and 1.6 d digits, hulled."""
    with precision(d):
        first = make(IVOps if rigor == 'interval' else MPOps)
        if rigor == 'interval': return first
        with precision(int(d*1.6)):
            second = make(MPOps)
            return tuple(hull(mp.mpf(u), mp.mpf(v)) for u, v in zip(first, second))

def sign_verdict(value, side):
    lo, hi = mp.mpf(value.a), mp.mpf(value.b)
    if side > 0: return 'holds' if lo > 0 else 'fails' if hi < 0 else 'open'
    return 'holds' if hi < 0 else 'fails' if lo > 0 else 'open'

def show(v, digits=15):
    if v is None: return None
    if isinstance(v, iv.mpf): return '[%s, %s]' % (mp.nstr(mp.mpf(v.a), digits), mp.nstr(mp.mpf(v.b), digits))
    if isinstance(v, float): return repr(v)
    return mp.nstr(v, digits)

def comparison(name, claim, verdict, hp, double=None, margin=None, rigor='interval', note='', info=None, **extra):
    return dict(name=name, claim=claim, verdict=verdict, hp=hp, double=double, margin=margin,
                rigor=rigor, note=note, info=info, **extra)

def settle(make, judge, rigor):
    """Evaluate make at increasing precision until judge(values) is not 'open'."""
    values = verdict = None; used = DPS_STEPS[0]
    for d in DPS_STEPS:
        values = enclose(make, rigor, d); verdict = judge(values); used = d
        if verdict != 'open': break
    return values, verdict, used

def root_offset(function, u0):
    """Numerical root of a monotone scalar function near u0 (secant at 50 digits): u0 - root, or None."""
    try:
        with precision(60):
            root = mp.findroot(function, (mp.mpf(u0), mp.mpf(u0)+mp.mpf(10)**-9), solver='secant', tol=mp.mpf(10)**-35, maxsteps=60, verify=False)
            return mp.mpf(u0)-root
    except Exception:
        return None

def bracket(name, claim, make, orient, rigor, source, width, *, certified=True, role='judged', numeric=None, u0=None,
            double=None, margin=None, note='', rel_offset=True):
    """Two signs at the ends of a claimed bracket.

    make(A) -> (left, right) values of the score at the claimed ends. orient = -1: the score decreases through its
    root (left positive, right negative); +1: increases. role 'info' reports the verdict without judging it."""
    judge = lambda v: ('fails' if 'fails' in (sign_verdict(v[0], -orient), sign_verdict(v[1], orient)) else
                       'holds' if sign_verdict(v[0], -orient) == sign_verdict(v[1], orient) == 'holds' else 'open')
    values, verdict, used = settle(make, judge, rigor)
    off = ''
    if numeric is not None and u0 is not None:
        o = root_offset(numeric, u0)
        if o is not None: off = '; hp offset (returned - root) = %s' % mp.nstr(o, 3)
    text = '%s; width %s (%s)%s%s' % (note, width, source, '; %d digits' % used if used != DPS_STEPS[0] else '', off)
    comp = comparison(name, claim, verdict if role == 'judged' else 'info', 'left %s, right %s' % (show(values[0]), show(values[1])),
                      double=double, margin=margin, rigor=rigor, note=text.lstrip('; '))
    comp['judged_verdict'] = verdict
    return comp

def estimate_of(r, name): return number(r['result']['estimates'][name])

def kind_of(r):
    o = r['result'].get('outcome') or {}
    return o.get('kind'), o.get('certified')

def mid(v):
    return mp.mpf(v.a)/2+mp.mpf(v.b)/2


# ------------------------------------------------------------ tolerances read from the bindings (default controls)

TOL = {
    'kumar': (1e-10, 'bindings/common/distributions/kumar/fit.hpp:7 reltol'),
    'beta': (1e-10, 'bindings/common/distributions/beta/fit.hpp:7 tol'),
    'nsbeta': (1e-10, 'bindings/common/distributions/nsbeta/fit.hpp:7 reltol'),
    'gamma': (1e-10, 'bindings/common/distributions/gamma/fit.hpp:5 tol'),
    'invgamma': (1e-10, 'bindings/common/distributions/invgamma/fit.hpp:9 tol'),
    'lgamma': (1e-10, 'bindings/common/distributions/lgamma/fit.hpp:9 tol'),
    'naka': (1e-10, 'bindings/common/distributions/naka/fit.hpp:9 tol'),
    'gumbel': (1e-10, 'bindings/common/distributions/gumbel/fit.hpp:9 reltol'),
    'weibull': (1e-10, 'bindings/common/distributions/weibull/fit.hpp:9 reltol'),
    'invweibull': (1e-10, 'bindings/common/distributions/invweibull/fit.hpp:9 tol'),
    'llogis': (1e-10, 'bindings/common/distributions/llogis/fit.hpp:9 tol'),
    'pareto3': (1e-10, 'bindings/common/distributions/pareto3/fit.hpp:7 reltol'),
    'logis': (1e-10, 'bindings/common/distributions/logis/fit.hpp:8 tol'),
    'dnorm': (1e-10, 'bindings/common/distributions/dnorm/fit.hpp:7 tol'),
    'dweibull': (1e-12, 'bindings/common/distributions/dweibull/fit.hpp:9 reltol'),
    'zeta': (1e-10, 'bindings/common/distributions/zeta/fit.hpp:5 reltol'),
    'yulesimon': (1e-10, 'bindings/common/distributions/yulesimon/fit.hpp:5 reltol'),
    'pig': (1e-10, 'bindings/common/distributions/pig/fit.hpp:5 reltol'),
    'genbeta': (1e-10, 'bindings/common/distributions/genbeta/fit.hpp:5 gradtol'),
    'frechet': (1e-10, 'bindings/common/distributions/frechet/fit.hpp:5 reltol'),
    'gev': (1e-10, 'bindings/common/distributions/gev/fit.hpp:7 reltol'),
    'route': (1e-10, 'bindings/common/modifiers.hpp:65 tol'),
}

def free_flag(r, bit): return not (r['mask'] >> bit) & 1


# ------------------------------------------------------------ Gumbel and Weibull (C)

def gumbel_F(A, d, s):
    """F(s) = s - mean(d) + sum d exp(-d/s) / sum exp(-d/s), d = x - min x (profiled scale equation,
    gumbel/mle.hpp:93; derived from the dossier log_density with mu profiled out). F increases in s."""
    n = len(d)
    e = [A.exp(-v/s) for v in d]
    return s-sum(d, A.mpf(0))/n+sum((v*w for v, w in zip(d, e)), A.mpf(0))/sum(e, A.mpf(0))

GUMBEL_ACCURACY = 1e-14    # min(.01 tol, max(.0001 tol, 4 eps)) at tol = 1e-10: gumbel/mle.hpp:108
CLAIMS['gumbel'] = {'free': ('relative bracket %.0e in sigma, half width %.0e' % (GUMBEL_ACCURACY, GUMBEL_ACCURACY/2), 'gumbel/mle.hpp:108,115'),
                    'mu held': ('bracket %.0e in 1/sigma, half width %.1e' % (1e-12, 5e-13), 'gumbel/mle.hpp:188 (.01 tol)')}

def replay_gumbel(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate': return [], None
    if not free_flag(r, 1): return [], 'closed form or evaluation (sigma held)'
    sigma = estimate_of(r, 'sigma'); x = [float(v) for v in data]
    if free_flag(r, 0):
        w = GUMBEL_ACCURACY/2+HALF
        m = min(x)
        def make(A):
            d = [A.mpf(v)-A.mpf(m) for v in x]; s = A.mpf(sigma)
            return gumbel_F(A, d, s*(1-A.mpf(w))), gumbel_F(A, d, s*(1+A.mpf(w)))
        def numeric(u):
            d = [mp.mpf(v)-mp.mpf(m) for v in x]; return gumbel_F(MPOps, d, mp.exp(u))
        return [bracket('root', 'F(sigma(1-w)) < 0 < F(sigma(1+w)), w = 0.5e-14 + rounding', make, +1, 'interval',
                        CLAIMS['gumbel']['free'][1], CLAIMS['gumbel']['free'][0], numeric=numeric, u0=mp.log(mp.mpf(sigma)))], None
    mu = estimate_of(r, 'mu'); t0 = 1/sigma; w = 0.5e-12+HALF
    def G(A, d, t):
        n = len(d)
        return A.mpf(n)/t-sum((v*(-A.expm1(-t*v)) for v in d), A.mpf(0))
    def make(A):
        d = [A.mpf(v)-A.mpf(mu) for v in x]; t = A.mpf(1)/A.mpf(sigma)
        return G(A, d, t*(1-A.mpf(w))), G(A, d, t*(1+A.mpf(w)))
    def numeric(u):
        d = [mp.mpf(v)-mp.mpf(mu) for v in x]; return G(MPOps, d, mp.exp(u))
    return [bracket('root', 'G(t(1-w)) > 0 > G(t(1+w)), t = 1/sigma, G = n/t - sum d (1 - exp(-t d)), w = 0.5e-12 + rounding', make, -1,
                    'interval', CLAIMS['gumbel']['mu held'][1], CLAIMS['gumbel']['mu held'][0], numeric=numeric, u0=mp.log(mp.mpf(t0)))], None

CLAIMS['weibull'] = {'free': ('relative bracket %.0e in 1/shape (Gumbel on -log x), half width %.0e' % (GUMBEL_ACCURACY, GUMBEL_ACCURACY/2), 'weibull/mle.hpp:73 -> gumbel/mle.hpp:108,115'),
                     'scale held': ('log shape bracket %.0e, half width %.1e' % (1e-12, 5e-13), 'weibull/mle.hpp:177-189 (.01 tol)')}

def replay_weibull(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate': return [], None
    if not free_flag(r, 0): return [], 'closed form or evaluation (shape held)'
    shape = estimate_of(r, 'shape'); x = [float(v) for v in data]
    if free_flag(r, 1):
        s0 = 1/shape; w = GUMBEL_ACCURACY/2+HALF
        top = max(x)
        def make(A):
            lt = A.log(A.mpf(top))
            d = [lt-A.log(A.mpf(v)) for v in x]; s = A.mpf(1)/A.mpf(shape)
            return gumbel_F(A, d, s*(1-A.mpf(w))), gumbel_F(A, d, s*(1+A.mpf(w)))
        def numeric(u):
            lt = mp.log(mp.mpf(top)); d = [lt-mp.log(mp.mpf(v)) for v in x]; return gumbel_F(MPOps, d, mp.exp(u))
        return [bracket('root', 'F(s(1-w)) < 0 < F(s(1+w)), s = 1/shape, F the Gumbel scale equation of y = -log x, w = 0.5e-14 + rounding',
                        make, +1, 'interval', CLAIMS['weibull']['free'][1], CLAIMS['weibull']['free'][0], numeric=numeric, u0=mp.log(mp.mpf(s0)))], None
    scale = float(r['held']['scale']); w = 0.5e-12+HALF
    def score(A, z, k):
        return A.mpf(len(z))/k-sum((v*A.expm1(k*v) for v in z), A.mpf(0))
    def make(A):
        z = [A.log(A.mpf(v)/A.mpf(scale)) for v in x]; k = A.mpf(shape)
        return score(A, z, k*A.exp(-A.mpf(w))), score(A, z, k*A.exp(A.mpf(w)))
    def numeric(u):
        z = [mp.log(mp.mpf(v)/mp.mpf(scale)) for v in x]; return score(MPOps, z, mp.exp(u))
    return [bracket('root', 'g(k e^-w) > 0 > g(k e^w), g = n/k - sum z expm1(k z), z = log(x/scale), w = 0.5e-12 + rounding', make, -1, 'interval',
                    CLAIMS['weibull']['scale held'][1], CLAIMS['weibull']['scale held'][0], numeric=numeric, u0=mp.log(mp.mpf(shape)))], None


# ------------------------------------------------------------ Kumaraswamy (C)

CLAIMS['kumar'] = {'a free (b free or held)': ('relative bracket %.0e in a (.01 tol), half width %.0e' % (1e-12, 5e-13), 'kumar/certificate.hpp:89 (hi-lo <= .01 tol lo), a inside the bracket, tol = min(tol, 1e-10) :108')}

def kumar_profile(A, x, a, fixed_b):
    """s(a) = 1 - a T + (b-1) D with T = -mean log x, D = mean t/expm1(t), t = -a log x and b = 1/G, G = mean(-log(1-x^a))
    (kumar/certificate.hpp:34-49; score of the profile in log a). Returns (s, b)."""
    n = len(x)
    lx = [A.log(v) for v in x]
    T = -sum(lx, A.mpf(0))/n
    G = sum((-A.log1p(-A.exp(a*v)) for v in lx), A.mpf(0))/n    # -log(1 - x^a), x^a = exp(a log x)
    D = sum((a*(-v)/A.expm1(-a*v) for v in lx), A.mpf(0))/n
    b = A.mpf(1)/G if fixed_b is None else A.mpf(fixed_b)
    return 1-a*T+(b-1)*D, b

def replay_kumar(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate': return [], None
    if not free_flag(r, 0): return [], 'closed form or evaluation (a held)'
    a_hat = estimate_of(r, 'a'); b_hat = estimate_of(r, 'b'); x = [float(v) for v in data]
    held_b = None if free_flag(r, 1) else float(r['held']['b'])
    w = 0.5e-12+HALF
    def make(A):
        X = [A.mpf(v) for v in x]; a = A.mpf(a_hat)
        return kumar_profile(A, X, a*(1-A.mpf(w)), held_b)[0], kumar_profile(A, X, a*(1+A.mpf(w)), held_b)[0]
    def numeric(u):
        X = [mp.mpf(v) for v in x]; return kumar_profile(MPOps, X, mp.exp(u), held_b)[0]
    out = [bracket('root', 's(a(1-w)) > 0 > s(a(1+w)), s = 1 - aT + (b-1)D, w = 0.5e-12 + rounding', make, -1, 'interval',
                   CLAIMS['kumar']['a free (b free or held)'][1], CLAIMS['kumar']['a free (b free or held)'][0],
                   numeric=numeric, u0=mp.log(mp.mpf(a_hat)))]
    if held_b is None:   # b enclosure: relative spread of b(a) over the claimed bracket (certificate.hpp:90, .01 tol each way)
        def make_b(A):
            X = [A.mpf(v) for v in x]; a = A.mpf(a_hat)
            bl = kumar_profile(A, X, a*(1-A.mpf(w)), None)[1]; bh = kumar_profile(A, X, a*(1+A.mpf(w)), None)[1]
            return abs(bh-bl)/(bl if mp.mpf(bl.a) < mp.mpf(bh.a) else bh) - A.mpf(2e-12), (A.mpf(b_hat)-bl)/bl, (bh-A.mpf(b_hat))/bh
        values, _, used = settle(make_b, lambda v: 'holds' if mp.mpf(v[0].b) <= 0 else 'fails' if mp.mpf(v[0].a) > 0 else 'open', 'interval')
        verdict = 'holds' if mp.mpf(values[0].b) <= 0 else 'fails' if mp.mpf(values[0].a) > 0 else 'open'
        verdict = 'info'   # not judged: the core's final bracket is usually far narrower than the box a(1 -/+ w), so the box spread says nothing about the b claim
        out.append(comparison('root-b', 'spread of b(a) over the box a(1 -/+ w) against 2 x .01 tol min b (information)', verdict,
                              'spread - 2e-12 = %s' % show(values[0]), note='kumar/certificate.hpp:90 (.01 tol on b); returned b within [b(a(1-w)), b(a(1+w))]: left %s right %s' % (
                                  mp.nstr(mid(values[1]), 3), mp.nstr(mid(values[2]), 3)), rigor='interval'))
    return out, None


# ------------------------------------------------------------ hcauchy, lgser, yulesimon (C)

CLAIMS['hcauchy'] = {'sigma free (mu free or held)': ('log sigma bracket 2e-14 (1+|log sigma|), half width 1e-14 (1+|log sigma|)', 'ht/mle.hpp:36-37 (hi-lo < 2e-14 (1+|mid|)), ls = midpoint, via hcauchy/mle.hpp:308 ht_mle_zero_legacy nu = 1')}

def replay_hcauchy(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate' or not free_flag(r, 1): return [], 'no root comparison'
    sigma = estimate_of(r, 'sigma'); mu = estimate_of(r, 'mu'); x = [float(v) for v in data]
    u0 = math.log(sigma); w = 1e-14*(1+abs(u0))+EPS+max(0.0, 0.5/(sigma/4.9406564584124654e-324)-EPS)   # public rounding: half spacing of sigma, absolute below the normal range
    positive = [v for v in x if v-mu > 0]
    n = len(x)
    def score(A, y, u):
        s2 = A.exp(2*u)
        return -A.mpf(n)+2*sum((v*v/(v*v+s2) for v in y), A.mpf(0))
    def make(A):
        y = [A.mpf(v)-A.mpf(mu) for v in x if v-mu > 0]; u = A.log(A.mpf(sigma))
        return score(A, y, u-A.mpf(w)), score(A, y, u+A.mpf(w))
    def numeric(u):
        y = [mp.mpf(v)-mp.mpf(mu) for v in x if v-mu > 0]; return score(MPOps, y, u)
    return [bracket('root', "h'(log sigma - w) > 0 > h'(log sigma + w), h' = -n + 2 sum y^2/(y^2 + sigma^2) over y = x - mu > 0, w = 1e-14 (1+|log sigma|) + rounding",
                    make, -1, 'interval', CLAIMS['hcauchy']['sigma free (mu free or held)'][1], CLAIMS['hcauchy']['sigma free (mu free or held)'][0],
                    numeric=numeric, u0=mp.log(mp.mpf(sigma)))], None

CLAIMS['lgser'] = {'theta free': ('relative bracket 4 eps in u = -log(1-theta), half width 2 eps + rounding of theta', 'lgser/mle.hpp:69 (hi/lo - 1 <= 4 eps), u = sqrt(lo) sqrt(hi) :73')}

def replay_lgser(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate' or not free_flag(r, 0): return [], 'no root comparison'
    theta = estimate_of(r, 'theta'); x = [int(v) for v in data]
    n = len(x); S = sum(x)
    delta = Fraction(S-n, n)
    def make(A):
        th = A.mpf(theta); u = -A.log1p(-th)
        # rounding of the public theta in u: one ulp of theta, relative to u
        rep = 2*HALF*th/((1-th)*u)
        w = A.mpf(2*EPS)+rep
        d = A.mpf(delta.numerator)/A.mpf(delta.denominator)
        q = lambda uu: (A.expm1(uu)-uu)/uu
        return q(u*(1-w))-d, q(u*(1+w))-d
    def numeric(uu):
        d = mp.mpf(delta.numerator)/delta.denominator; e = mp.exp(uu); return (mp.expm1(e)-e)/e-d
    u_hp = -mp.log1p(-mp.mpf(theta))
    return [bracket('root', 'q(u(1-w)) < delta < q(u(1+w)), q(u) = (expm1(u) - u)/u, delta = (sum x - n)/n, w = 2 eps + 2 half-ulp(theta)/((1-theta)u)',
                    make, +1, 'interval', CLAIMS['lgser']['theta free'][1], CLAIMS['lgser']['theta free'][0],
                    numeric=numeric, u0=mp.log(u_hp))], None

CLAIMS['yulesimon'] = {'rho free': ('log rho bracket 1e-10 (one end is the returned point)', 'yulesimon/mle.hpp:239 (hi-lo <= tol), rho = exp(mid), mid an end of the bracket :235')}

def replay_yulesimon(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate' or not free_flag(r, 0): return [], 'no root comparison'
    rho = estimate_of(r, 'rho'); x = [int(v) for v in data]
    n = len(x); counts = collections.Counter(x)
    w = TOL['yulesimon'][0]+EPS
    big = max(counts) > 3000
    def score(A, lr):
        rr = A.exp(lr); out = A.mpf(1)
        run = A.mpf(0); j = 0
        for k in sorted(counts):
            if big:   # numerical: rho (psi(rho + k + 1) - psi(rho + 1))
                term = rr*(A.digamma(rr+k+1)-A.digamma(rr+1))
            else:
                while j < k: j += 1; run += rr/(rr+j)
                term = run
            out -= A.mpf(counts[k])/n*term
        return out
    def make(A):
        lr = A.log(A.mpf(rho)); return score(A, lr-A.mpf(w)), score(A, lr+A.mpf(w))
    def numeric(u): return score(MPOps, u)
    return [bracket('root', 's(log rho - w) > 0 > s(log rho + w), s = 1 - mean_i sum_{j<=x_i} rho/(rho+j), w = 1e-10 + rounding', make, -1,
                    'numerical' if big else 'interval', CLAIMS['yulesimon']['rho free'][1], CLAIMS['yulesimon']['rho free'][0],
                    numeric=numeric, u0=mp.log(mp.mpf(rho)))], None


# ------------------------------------------------------------ restored() through the normal sufficient statistics (B)

def restore_verdict(sup, ll, tol, magnitude_shift=0):
    """holds if sup - ll <= tol max(1, |sup|) + 128 eps (1 + |sup| + |ll|), from interval enclosures (core/transformed_data.hpp restored();
    magnitude_shift is the constant the core leaves out of both likelihoods when forming the allowance)."""
    gap = sup-ll
    s0, l0 = mp.mpf(sup.a)/2+mp.mpf(sup.b)/2-magnitude_shift, mp.mpf(ll.a)/2+mp.mpf(ll.b)/2-magnitude_shift
    bound = mp.mpf(tol)*max(1, abs(s0))+128*mp.mpf(EPS)*(1+abs(s0)+abs(l0))
    verdict = 'holds' if mp.mpf(gap.b) <= bound else 'fails' if mp.mpf(gap.a) > bound else 'open'
    return verdict, gap, bound

def restore_comparison(name, claim, make, tol, source, certified, shift=None, extra=''):
    """make(A) -> (sup, ll[, shift]) enclosures at the same constants as the core."""
    judge = lambda v: restore_verdict(v[0], v[1], tol, mid(v[2]) if len(v) > 2 else 0)[0]
    values, verdict, used = settle(make, judge, 'interval')
    verdict, gap, bound = restore_verdict(values[0], values[1], tol, mid(values[2]) if len(values) > 2 else 0)
    if certified is False:
        note = 'flagged uncertified by the core: gap within the bound: %s' % ('yes' if verdict == 'holds' else 'no')
        verdict = 'info'
    else:
        note = ''
    return comparison(name, claim, verdict, 'gap sup - ll = %s, bound %s' % (show(gap, 8), mp.nstr(bound, 8)), margin=mp.nstr(bound, 5),
                      rigor='interval', note='%s tol %g (%s)%s%s' % (note+'; ' if note else '', tol, source, '; %d digits' % used if used != DPS_STEPS[0] else '', extra))

def normal_logliks(A, ys, jac, mean, sd, n):
    """sum of log N(y_i; mean, sd) plus the Jacobian term jac (a constant)."""
    ss = sum(((v-mean)*(v-mean) for v in ys), A.mpf(0))
    return -n*(A.log(sd)+A.log(2*A.mpf(mp.pi))/2)-ss/(2*sd*sd)+jac

CLAIMS['lnorm'] = {'mean free': ('restored(ideal, ll, 1e-10, |ideal| + |ll|)', 'core/transformed_data.hpp:99-103,117 (transformed_detail::normal)')}

def replay_normal_transformed(r, data, budget, logit, names, family):
    kind, certified = kind_of(r)
    if kind != 'estimate': return [], None
    if not free_flag(r, 0): return [], 'mean held: no restoration check'
    mean_hat = estimate_of(r, names[0]); sd_hat = estimate_of(r, names[1]); x = [float(v) for v in data]
    n = len(x)
    held_sd = None if free_flag(r, 1) else float(r['held'][names[1]])
    def make(A):
        X = [A.mpf(v) for v in x]
        if logit:
            ys = [A.log(v/(1-v)) for v in X]; jac = -sum((A.log(v)+A.log1p(-v) for v in X), A.mpf(0))
        else:
            ys = [A.log(v) for v in X]; jac = -sum(ys, A.mpf(0))
        nn = A.mpf(n); ybar = sum(ys, A.mpf(0))/nn
        if held_sd is None:
            s2 = sum(((v-ybar)*(v-ybar) for v in ys), A.mpf(0))/nn
            sup = normal_logliks(A, ys, jac, ybar, A.sqrt(s2), nn)
        else:
            sup = normal_logliks(A, ys, jac, ybar, A.mpf(held_sd), nn)
        ll = normal_logliks(A, ys, jac, A.mpf(mean_hat), A.mpf(sd_hat), nn)
        return sup, ll
    return [restore_comparison('restore', 'sup - ll <= tol max(1,|ideal|) + 128 eps (1 + |ideal| + |ll|)', make, 1e-10,
                               CLAIMS['lnorm']['mean free'][1], certified)], None

def replay_lnorm(r, data, budget): return replay_normal_transformed(r, data, budget, False, ('meanlog', 'sdlog'), 'lnorm')
def replay_logitnorm(r, data, budget): return replay_normal_transformed(r, data, budget, True, ('mu', 'sigma'), 'logitnorm')



# ------------------------------------------------------------ numerical helpers (50 digits)

def decreasing_root(f, x0, step=1.0):
    """Root of a strictly decreasing mp function: expand a bracket from x0, then bisect to the working precision."""
    x0 = mp.mpf(x0); f0 = f(x0)
    if f0 == 0: return x0
    sign = 1 if f0 > 0 else -1
    lo = hi = x0; st = mp.mpf(step)
    for _ in range(400):
        nxt = lo-st if sign < 0 else hi+st
        fv = f(nxt)
        if (fv > 0) != (sign > 0) or fv == 0:
            if sign > 0: lo, hi = hi, nxt
            else: lo, hi = nxt, lo
            break
        if sign > 0: hi = nxt
        else: lo = nxt
        st *= 2
    else:
        return None
    # now f(lo) >= 0 >= f(hi) (decreasing)
    for _ in range(400):
        m = (lo+hi)/2
        if f(m) > 0: lo = m
        else: hi = m
        if hi-lo < mp.mpf(10)**(-mp.mp.dps+8)*max(1, abs(m)): break
    return (lo+hi)/2

def numgrad(f, theta, h=None):
    h = h or mp.mpf(10)**(-mp.mp.dps//3)
    out = []
    for i in range(len(theta)):
        up = list(theta); dn = list(theta); up[i] += h; dn[i] -= h
        out.append((f(*up)-f(*dn))/(2*h))
    return out

def numhess(f, theta, h=None):
    h = h or mp.mpf(10)**(-mp.mp.dps//4)
    k = len(theta); H = [[0]*k for _ in range(k)]
    for i in range(k):
        for j in range(i, k):
            def at(si, sj):
                t = list(theta); t[i] += si*h; t[j] += sj*h; return f(*t)
            v = (at(1, 1)-at(1, -1)-at(-1, 1)+at(-1, -1))/(4*h*h)
            H[i][j] = H[j][i] = v
    return H

def newton_point(grad, theta0, steps=12, hs=None):
    """Newton from theta0 on a smooth concave scalar field's gradient using a numerical Jacobian (50 digits)."""
    t = [mp.mpf(v) for v in theta0]
    for _ in range(steps):
        g = grad(*t)
        J = []
        for i in range(len(t)):
            h = mp.mpf(hs[i]) if hs else mp.mpf(10)**(-mp.mp.dps//3)
            up = list(t); dn = list(t); up[i] += h; dn[i] -= h
            gu, gd = grad(*up), grad(*dn)
            J.append([(gu[k]-gd[k])/(2*h) for k in range(len(t))])
        Jm = mp.matrix([[J[i][k] for i in range(len(t))] for k in range(len(t))])
        step = mp.lu_solve(Jm, mp.matrix(g))
        t = [t[i]-step[i] for i in range(len(t))]
        if max(abs(step[i]) for i in range(len(t))) < mp.mpf(10)**(-mp.mp.dps+12): break
    return t


# ------------------------------------------------------------ beta, nsbeta (C): profile root in logit mu, held-shape bisection

CLAIMS['beta'] = {
    'free': ('bracket 2 min(tol, needed) in eta = logit(mu), needed = max(4 eps max(1,|eta|), min(tol/(1+|tau slope|), sqrt(tol/(1+t mu (1-mu))))); tol = 1e-10',
             'beta/mle.hpp:226 (hi-lo <= 2 w), :421-443 (needed, solved; justification comment :412-416), :421 tol = max(tol, 1e-14)'),
    'one shape held': ('relative bracket 1e-13 in the free shape', 'beta/mle.hpp:315 (hi-lo <= 1e-13 z); z is an end of the bracket'),
}

def psi_inc_score(A, target, held, z):
    return target+A.digamma(z+held)-A.digamma(z)

def beta_free_roots(xs, mask_bits, r, names, tol, label, source):
    """Profile root in eta = logit(mu), both shapes free. xs(A) -> list of A numbers in (0,1)."""
    a_hat = estimate_of(r, names[0]); b_hat = estimate_of(r, names[1])
    tol_eff = max(tol, 1e-14)
    def ray_root(A, mu, L1, L2):
        mc = 1-mu
        f = lambda tau: (lambda t: A.digamma(t)-mu*A.digamma(mu*t)-mc*A.digamma(mc*t)+mu*L1+mc*L2)(mp.exp(tau))
        return decreasing_root(f, 0)
    def q(A, eta, L1, L2):
        mu = 1/(1+mp.exp(-eta)); mc = 1/(1+mp.exp(eta))
        t = mp.exp(ray_root(A, mu, L1, L2))
        return t*(-A.digamma(mu*t)+A.digamma(mc*t)+L1-L2)
    extra = int(2*max(0, math.log10(max(a_hat, b_hat)))+2*max(0, -math.log10(min(a_hat, b_hat)))+10)   # digits lost to the digamma differences at extreme shapes
    def make(A):
        with precision(mp.mp.dps+extra):
            return make_inner(A)
    def make_inner(A):
        X = xs(A); n = len(X)
        L1 = sum((A.log(v) for v in X), A.mpf(0))/n; L2 = sum((A.log1p(-v) for v in X), A.mpf(0))/n
        a, b = A.mpf(a_hat), A.mpf(b_hat); t = a+b; mu = a/t; mc = b/t; eta = A.log(a/b)
        ca = A.psi1(mu*t)-1/(mu*t); cb = A.psi1(mc*t)-1/(mc*t); ct = A.psi1(t)-1/t
        h_tau = t*(ct-mu*mu*ca-mc*mc*cb)
        slope = -mu*mc*t*(mc*cb-mu*ca)/h_tau
        floor = 4*mp.mpf(EPS)*max(1, abs(eta))
        needed = max(min(mp.mpf(tol_eff)/(1+abs(slope)), mp.sqrt(mp.mpf(tol_eff)/(1+t*mu*mc))), floor)
        W = 2*min(mp.mpf(tol_eff), needed)+2*mp.mpf(EPS)
        return q(A, eta-W, L1, L2), q(A, eta+W, L1, L2)
    def numeric(u):
        with precision(mp.mp.dps+extra):
            A = MPOps; X = xs(A); n = len(X)
            L1 = sum((A.log(v) for v in X), A.mpf(0))/n; L2 = sum((A.log1p(-v) for v in X), A.mpf(0))/n
            return q(A, u, L1, L2)
    return bracket('root', 'q(eta - W) > 0 > q(eta + W), eta = logit(mu), q the profile score dl/dmu at the ray root t(mu), W = 2 min(tol, needed) + rounding',
                   make, -1, 'numerical', source, CLAIMS['beta']['free'][0], numeric=numeric,
                   u0=mp.log(mp.mpf(a_hat)/mp.mpf(b_hat)), note=label)

def beta_held_root(xs, r, names, held_name, tol_w, label, source, width_text):
    """One shape held: score target + psi(z + held) - psi(z) decreasing in the free shape z."""
    free_name = names[1] if held_name == names[0] else names[0]
    z_hat = estimate_of(r, free_name); held = float(r['held'][held_name]) if held_name in r['held'] else estimate_of(r, held_name)
    def make(A):
        X = xs(A); n = len(X)
        target = sum(((A.log1p(-v) if held_name == names[0] else A.log(v)) for v in X), A.mpf(0))/n
        z = A.mpf(z_hat); w = A.mpf(tol_w)
        return psi_inc_score(A, target, A.mpf(held), z*(1-w)), psi_inc_score(A, target, A.mpf(held), z*(1+w))
    def numeric(u):
        A = MPOps; X = xs(A); n = len(X)
        target = sum(((A.log1p(-v) if held_name == names[0] else A.log(v)) for v in X), A.mpf(0))/n
        return psi_inc_score(A, target, A.mpf(held), mp.exp(u))
    return bracket('root', 'g(z(1-w)) > 0 > g(z(1+w)), g = target + psi(z + held) - psi(z), w = %s' % tol_w, make, -1, 'numerical', source, width_text,
                   numeric=numeric, u0=mp.log(mp.mpf(z_hat)), note=label)

def replay_beta_like(r, xs, names, tol, label, shift_bits=0):
    """names = (shape1, shape2); shift_bits: bits of the mask above the two shapes (nsbeta: min, max held)."""
    kind, _ = kind_of(r)
    if kind != 'estimate': return [], None
    m = (r['mask'] >> 0) & 3
    if m == 3: return [], 'evaluation only'
    src = CLAIMS['beta']
    if m == 0: return [beta_free_roots(xs, m, r, names, tol, label, src['free'][1])], None
    held_name = names[0] if m == 1 else names[1]
    return [beta_held_root(xs, r, names, held_name, 1e-13, label, src['one shape held'][1], src['one shape held'][0])], None

def replay_beta(r, data, budget):
    x = [float(v) for v in data]
    return replay_beta_like(r, lambda A: [A.mpf(v) for v in x], ('shape1', 'shape2'), TOL['beta'][0], 'beta')

def replay_nsbeta(r, data, budget):
    x = [float(v) for v in data]; lo = float(r['held']['min']); hi = float(r['held']['max'])
    return replay_beta_like(r, lambda A: [(A.mpf(v)-A.mpf(lo))/(A.mpf(hi)-A.mpf(lo)) for v in x], ('shape1', 'shape2'), TOL['nsbeta'][0], 'nsbeta on (x - min)/(max - min)')


# ------------------------------------------------------------ gamma, invgamma, lgamma, naka

CLAIMS['gamma'] = {
    'free, Newton path (s >= 1e-3)': ('relative Newton step at most tol = 1e-10, applied, no bracket', 'gamma/mle.hpp:176-196 (newton_raphson_1d, core/newton.hpp)'),
    'free, bracket path': ('relative bracket 4 eps, half width 2 eps', 'gamma/mle.hpp:204-211 (hi/lo - 1 <= 4 eps), shape = sqrt(lo) sqrt(hi)'),
    'scale held': ('log shape bracket 1e-12, half width 5e-13', 'gamma/mle.hpp:263-270 (hi - lo > 1e-12)'),
}

def gamma_G(A, k, s): return A.log(k)-A.digamma(k)-s

def replay_gamma(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate': return [], None
    x = [float(v) for v in data]; shape = estimate_of(r, 'shape'); abstol = TOL['gamma'][0]  # relative, see CLAIMS
    if r['mask'] == 0:
        def stats(A):
            X = [A.mpf(v) for v in x]; n = len(X)
            return A.log(sum(X, A.mpf(0))/n)-sum((A.log(v) for v in X), A.mpf(0))/n
        with precision(60):
            s_hp = stats(MPOps)
        newton = s_hp >= mp.mpf('1e-3') and min(x) > 0 and (sum(x)/len(x)) >= 2.2250738585072014e-308
        if newton:
            def make(A):
                s = stats(A); k = A.mpf(shape); w = A.mpf(abstol)*k
                return gamma_G(A, k-w, s), gamma_G(A, k+w, s)
            src, width = CLAIMS['gamma']['free, Newton path (s >= 1e-3)']
            claim = 'G(k (1 - tol)) > 0 > G(k (1 + tol)), G = log k - psi(k) - s, s = log mean - mean log (Newton path, relative step tol)'
        else:
            w = 2*EPS+HALF
            def make(A):
                s = stats(A); k = A.mpf(shape)
                return gamma_G(A, k*(1-A.mpf(w)), s), gamma_G(A, k*(1+A.mpf(w)), s)
            src, width = CLAIMS['gamma']['free, bracket path']
            claim = 'G(k(1-w)) > 0 > G(k(1+w)), w = 2 eps + rounding (bracket path)'
        def numeric(u):
            return gamma_G(MPOps, mp.exp(u), stats(MPOps))
        out = [bracket('root', claim, make, -1, 'numerical', src, width, numeric=numeric, u0=mp.log(mp.mpf(shape)))]
        if newton:   # accuracy contract of the 2.0 default tol 1e-10 (information)
            def make_c(A):
                s = stats(A); k = A.mpf(shape); w = A.mpf(1e-10)*max(1, k)/k
                return gamma_G(A, k*(1-w), s), gamma_G(A, k*(1+w), s)
            c = bracket('root-tol', 'G(k - 1e-10 max(1,k)) > 0 > G(k + 1e-10 max(1,k)) (accuracy contract, information)', make_c, -1, 'numerical',
                        'accuracy-contract.md', '1e-10 relative', role='info')
            out.append(c)
        return out, None
    if r['mask'] == 2:   # scale held, shape free
        scale = float(r['held']['scale'])
        w = 0.5e-12+HALF
        def mk(A):
            X = [A.mpf(v) for v in x]; n = len(X)
            target = A.log(A.mpf(1)/A.mpf(scale))+sum((A.log(v) for v in X), A.mpf(0))/n
            k = A.mpf(shape)
            return A.digamma(k*A.exp(-A.mpf(w)))-target, A.digamma(k*A.exp(A.mpf(w)))-target
        def numeric(u):
            A = MPOps; X = [A.mpf(v) for v in x]; n = len(X)
            return A.digamma(mp.exp(u))-(A.log(A.mpf(1)/A.mpf(scale))+sum((A.log(v) for v in X), A.mpf(0))/n)
        return [bracket('root', 'psi(k e^-w) < target < psi(k e^w), target = log(1/scale) + mean log x, w = 0.5e-12 + rounding', mk, +1, 'numerical',
                        CLAIMS['gamma']['scale held'][1], CLAIMS['gamma']['scale held'][0], numeric=numeric, u0=mp.log(mp.mpf(shape)))], None
    return [], 'closed form or evaluation'

def gamma_family_maps(kind):
    """(Y list builder, jacobian builder, public log density) for the gamma-through-transformation families."""
    if kind == 'invgamma':
        ys = lambda A, X: [1/v for v in X]
        jac = lambda A, X: -2*sum((A.log(v) for v in X), A.mpf(0))
        ll = lambda A, X, a, b: sum((a*A.log(b)-A.loggamma(a)-(a+1)*A.log(v)-b/v for v in X), A.mpf(0))
    elif kind == 'naka':
        ys = lambda A, X: [v*v for v in X]
        jac = lambda A, X: sum((A.log(A.mpf(2))+A.log(v) for v in X), A.mpf(0))
        ll = lambda A, X, k, om: sum((A.log(A.mpf(2))+k*A.log(k)-A.loggamma(k)-k*A.log(om)+(2*k-1)*A.log(v)-k*v*v/om for v in X), A.mpf(0))
    else:
        ys = lambda A, X: [A.log(v) for v in X]
        jac = lambda A, X: -sum((A.log(v) for v in X), A.mpf(0))
        ll = lambda A, X, k, rt: sum((k*A.log(rt)-A.loggamma(k)+(k-1)*A.log(A.log(v))-(rt+1)*A.log(v) for v in X), A.mpf(0))
    return ys, jac, ll

CLAIMS['gamma_raw'] = {
    'free parameter': ('restored(ideal, ll, tol, |ideal| + |ll|) with tol = 1e-10; the shape bracket (relative 4 eps) is not what certifies',
                       'gamma/transformed.hpp:143 (certified = !free_parameter || restored), :90 (hi/lo - 1 <= min(tol/8, 4 eps)); core/transformed_data.hpp:99'),
    'parameter held': ('shape: log bracket 2e-13 (Reciprocal, Log: digamma equation), relative 4 eps (Square: log k - psi(k) = s)',
                       'gamma/transformed.hpp:105-110 (hi - lo <= min(tol/8, 2e-13)), :74-90 (Square)'),
}

def replay_gamma_raw(kind_name):
    def replay(r, data, budget):
        kind, certified = kind_of(r)
        if kind != 'estimate': return [], None
        names = {'invgamma': ('alpha', 'beta'), 'naka': ('shape', 'scale'), 'lgamma': ('shapelog', 'ratelog')}[kind_name]
        x = [float(v) for v in data]; shape_hat = estimate_of(r, names[0]); par_hat = estimate_of(r, names[1])
        ys, jacf, llf = gamma_family_maps(kind_name)
        tol = TOL[kind_name][0]
        if r['mask'] == 0:
            def make(A):
                X = [A.mpf(v) for v in x]; n = A.mpf(len(X)); Y = ys(A, X)
                ybar = sum(Y, A.mpf(0))/n; lm = sum((A.log(v) for v in Y), A.mpf(0))/n
                s = A.log(ybar)-lm
                k = mp.exp(decreasing_root(lambda tau: A.log(mp.exp(tau))-A.digamma(mp.exp(tau))-s, 0, 1))
                sup = n*(k*A.log(k/ybar)-A.loggamma(k)+(k-1)*lm-k)+jacf(A, X)
                ll = llf(A, X, A.mpf(shape_hat), A.mpf(par_hat))
                return sup, ll
            judge = lambda v: restore_verdict(v[0], v[1], tol)[0]
            values, verdict, used = settle(make, judge, 'numerical')
            verdict, gap, bound = restore_verdict(values[0], values[1], tol)
            note = 'tol %g (%s)' % (tol, TOL[kind_name][1])
            if certified is False:
                note = 'flagged uncertified by the core: gap within the bound: %s; %s' % ('yes' if verdict == 'holds' else 'no', note); verdict = 'info'
            out = [comparison('restore', 'sup - ll <= tol max(1,|ideal|) + 128 eps (1 + |ideal| + |ll|), sup the 50-digit maximizer', verdict,
                              'gap sup - ll = %s, bound %s' % (show(gap, 8), mp.nstr(bound, 8)), margin=mp.nstr(bound, 5), rigor='numerical',
                              note='%s; %s' % (note, CLAIMS['gamma_raw']['free parameter'][1]))]
            # shape root (information): the bracket is 4 eps relative; for lgamma the returned shape is the allowance-checked delegate (1e-12)
            w = (2*EPS+HALF) if kind_name != 'lgamma' else (1e-12+2*EPS+HALF)
            def mk(A):
                X = [A.mpf(v) for v in x]; n = A.mpf(len(X)); Y = ys(A, X)
                s = A.log(sum(Y, A.mpf(0))/n)-sum((A.log(v) for v in Y), A.mpf(0))/n
                k = A.mpf(shape_hat)
                return gamma_G(A, k*(1-A.mpf(w)), s), gamma_G(A, k*(1+A.mpf(w)), s)
            def numeric(u):
                A = MPOps; X = [A.mpf(v) for v in x]; n = A.mpf(len(X)); Y = ys(A, X)
                s = A.log(sum(Y, A.mpf(0))/n)-sum((A.log(v) for v in Y), A.mpf(0))/n
                return gamma_G(A, mp.exp(u), s)
            out.append(bracket('root', 'G(k(1-w)) > 0 > G(k(1+w)), G = log k - psi(k) - s on the transformed data, w = %s' % ('2 eps' if kind_name != 'lgamma' else '1e-12 + 2 eps'),
                               mk, -1, 'numerical', CLAIMS['gamma_raw']['free parameter'][1], 'relative bracket 4 eps (half width 2 eps)', role='info',
                               numeric=numeric, u0=mp.log(mp.mpf(shape_hat))))
            return out, None
        if r['mask'] == 2:   # parameter held, shape free
            par = float(r['held'][names[1]])
            if kind_name == 'naka':
                w = 2*EPS+HALF
                def mk(A):
                    X = [A.mpf(v) for v in x]; n = A.mpf(len(X)); Y = ys(A, X); om = A.mpf(par)
                    ybar = sum(Y, A.mpf(0))/n; lm = sum((A.log(v) for v in Y), A.mpf(0))/n
                    s = A.log(om)-lm+ybar/om-1
                    k = A.mpf(shape_hat)
                    return gamma_G(A, k*(1-A.mpf(w)), s), gamma_G(A, k*(1+A.mpf(w)), s)
                def numeric(u):
                    A = MPOps; X = [A.mpf(v) for v in x]; n = A.mpf(len(X)); Y = ys(A, X); om = A.mpf(par)
                    s = A.log(om)-sum((A.log(v) for v in Y), A.mpf(0))/n+sum(Y, A.mpf(0))/n/om-1
                    return gamma_G(A, mp.exp(u), s)
                claim = 'G(k(1-w)) > 0 > G(k(1+w)), G = log k - psi(k) - s, s = log(Omega) - mean log x^2 + mean x^2/Omega - 1, w = 2 eps + rounding'
                return [bracket('root', claim, mk, -1, 'numerical', CLAIMS['gamma_raw']['parameter held'][1], CLAIMS['gamma_raw']['parameter held'][0],
                                numeric=numeric, u0=mp.log(mp.mpf(shape_hat)))], None
            w = 1e-13+2*HALF if kind_name == 'invgamma' else 1e-12+1e-13+2*HALF
            def mk(A):
                X = [A.mpf(v) for v in x]; n = A.mpf(len(X)); Y = ys(A, X)
                target = A.log(A.mpf(par))+sum((A.log(v) for v in Y), A.mpf(0))/n
                k = A.mpf(shape_hat)
                return A.digamma(k*A.exp(-A.mpf(w)))-target, A.digamma(k*A.exp(A.mpf(w)))-target
            def numeric(u):
                A = MPOps; X = [A.mpf(v) for v in x]; n = A.mpf(len(X)); Y = ys(A, X)
                return A.digamma(mp.exp(u))-(A.log(A.mpf(par))+sum((A.log(v) for v in Y), A.mpf(0))/n)
            return [bracket('root', 'psi(k e^-w) < log(parameter) + mean log Y < psi(k e^w), w = %s' % ('1e-13 + rounding' if kind_name == 'invgamma' else '1e-12 (delegate allowance) + 1e-13 + rounding'),
                            mk, +1, 'numerical', CLAIMS['gamma_raw']['parameter held'][1], CLAIMS['gamma_raw']['parameter held'][0],
                            numeric=numeric, u0=mp.log(mp.mpf(shape_hat)))], None
        return [], 'closed form or evaluation'
    return replay

for _k in ('invgamma', 'naka', 'lgamma'): pass


# ------------------------------------------------------------ llogis, invweibull, pareto3 (location_fit): restore and the log-shape root

CLAIMS['location'] = {
    'free scale': ('restored(ideal, ll, tol, |ideal| + |ll|) on the log-data likelihood (no Jacobian), tol = the binding control; the shape/location roots (4 eps absolute in log b and in c) do not certify',
                   'core/transformed_location.hpp:75-77 (certified = !free_scale || restored), :16-19 (root width min(tol/16, 4 eps))'),
    'scale held': ('log shape bracket 4 eps (absolute), half width 2 eps, plus conversion rounding', 'core/transformed_location.hpp:16-19,51 (root in z = log b, hi - lo <= min(tol/16, 4 eps))'),
}

def loc_ll(A, W, kind_name, k, ls):
    """log-data likelihood sum[log k + tail], tail the Gumbel (invweibull) or logistic (llogis) term at u = k (w - log s)."""
    n = len(W)
    if kind_name == 'invweibull':
        return sum((A.log(k)-(k*(w-ls))-A.exp(-(k*(w-ls))) for w in W), A.mpf(0))
    return sum((A.log(k)+(k*(w-ls))-2*A.log1p(A.exp(k*(w-ls))) for w in W), A.mpf(0))

def loc_sup(A, W, kind_name, k_fixed, ls_fixed, k0, ls0):
    """sup of loc_ll over the free coefficients (k_fixed / ls_fixed None = free), numerically at the working precision."""
    n = A.mpf(len(W))
    sw = sum(W, A.mpf(0))
    def c_of(k):   # c = k log s: root in c given k (Newton on the strictly concave c-profile)
        if kind_name == 'invweibull':
            return A.log(n/sum((A.exp(-k*w) for w in W), A.mpf(0)))
        c = sum((k*w for w in W), A.mpf(0))/n
        for _ in range(60):
            P = [1/(1+A.exp(-(k*w-c))) for w in W]
            f = -n+2*sum(P, A.mpf(0)); df = -2*sum((p*(1-p) for p in P), A.mpf(0))
            step = f/df; c -= step
            if abs(step) < mp.mpf(10)**(-mp.mp.dps+10)*max(1, abs(c)): break
        return c
    if k_fixed is not None:
        k = A.mpf(k_fixed)
        c = c_of(k)
        return loc_ll(A, W, kind_name, k, c/k)
    if ls_fixed is not None:   # scale held: root in k
        ls = A.mpf(ls_fixed)
        def dk(kk):
            u = [kk*(w-ls) for w in W]
            if kind_name == 'invweibull': return n/kk+sum(((w-ls)*(-1+A.exp(-uu)) for w, uu in zip(W, u)), A.mpf(0))
            return n/kk+sum(((w-ls)*(1-2/(1+A.exp(-uu))) for w, uu in zip(W, u)), A.mpf(0))
        k = mp.exp(decreasing_root(lambda tau: dk(mp.exp(tau)), mp.log(mp.mpf(k0)), 0.5))
        return loc_ll(A, W, kind_name, k, ls)
    def grad_k(kk):   # profile score in k at c(k)
        c = c_of(kk)
        if kind_name == 'invweibull':
            e = [A.exp(-kk*w) for w in W]
            return n/kk+n*sum((w*v for w, v in zip(W, e)), A.mpf(0))/sum(e, A.mpf(0))-sw
        return n/kk+sw-2*sum((w/(1+A.exp(-(kk*w-c))) for w in W), A.mpf(0))
    tau = mp.findroot(lambda t: grad_k(mp.exp(t)), (mp.log(mp.mpf(k0)), mp.log(mp.mpf(k0))+mp.mpf(10)**-8), solver='secant', tol=mp.mpf(10)**(-mp.mp.dps+12), maxsteps=80, verify=False)
    k = mp.exp(tau)
    c = c_of(k)
    return loc_ll(A, W, kind_name, k, c/k)

def replay_location(kind_name, family, data_fn, tol, tol_source, polish_width=None):
    """data_fn(r, data) -> (list of float for the observations x - offset, names of (shape, scale))."""
    def replay(r, data, budget):
        kind, certified = kind_of(r)
        if kind != 'estimate': return [], None
        x, shape_name, scale_name, shift_bits = data_fn(r, data)
        shape = estimate_of(r, shape_name); scale = estimate_of(r, scale_name)
        shape_held = bool((r['mask'] >> shift_bits) & 1); scale_held = bool((r['mask'] >> (shift_bits+1)) & 1)
        if shape_held and scale_held: return [], 'evaluation only'
        out = []
        W = lambda A: [A.log(v) for v in x]
        if not scale_held:
            def make(A):
                Wv = W(A)
                sup = loc_sup(A, Wv, kind_name, float(r['held'][shape_name]) if shape_held else None, None, shape, 0)
                return sup, loc_ll(A, Wv, kind_name, A.mpf(shape), A.log(A.mpf(scale))), A.mpf(0)
            # the core's allowance uses the log-data likelihood (no Jacobian): same quantity as loc_ll
            mk = lambda A: make(A)[:2]
            judge = lambda v: restore_verdict(v[0], v[1], tol)[0]
            values, verdict, used = settle(mk, judge, 'numerical')
            verdict, gap, bound = restore_verdict(values[0], values[1], tol)
            note = 'tol %g (%s)' % (tol, tol_source)
            if certified is False:
                note = 'flagged uncertified by the core: gap within the bound: %s; %s' % ('yes' if verdict == 'holds' else 'no', note); verdict = 'info'
            out.append(comparison('restore', 'sup - ll <= tol max(1,|ideal|) + 128 eps (1 + |ideal| + |ll|) on the log-data likelihood, sup the 50-digit maximizer', verdict,
                                  'gap sup - ll = %s, bound %s' % (show(gap, 8), mp.nstr(bound, 8)), margin=mp.nstr(bound, 5), rigor='numerical',
                                  note='%s; %s%s' % (note, CLAIMS['location']['free scale'][1], '; %d digits' % used if used != DPS_STEPS[0] else '')))
        if not shape_held:
            w = 4*EPS if polish_width is None else polish_width+4*EPS    # 2 eps (bracket half width) + chained rounding; pareto3: the accuracy polish (tol) replaces the bracket
            if scale_held:
                ls_value = float(r['held'][scale_name])
                def make_r(A):
                    Wv = W(A); n = A.mpf(len(Wv)); ls = A.log(A.mpf(ls_value)); k = A.mpf(shape)
                    def dk(kk):
                        if kind_name == 'invweibull': return n/kk+sum(((v-ls)*(-1+A.exp(-(kk*(v-ls)))) for v in Wv), A.mpf(0))
                        return n/kk+sum(((v-ls)*(1-2/(1+A.exp(-(kk*(v-ls))))) for v in Wv), A.mpf(0))
                    return dk(k*A.exp(-A.mpf(w))), dk(k*A.exp(A.mpf(w)))
                def numeric(u):
                    A = MPOps; Wv = W(A); n = A.mpf(len(Wv)); ls = A.log(A.mpf(ls_value)); kk = mp.exp(u)
                    if kind_name == 'invweibull': return n/kk+sum(((v-ls)*(-1+A.exp(-(kk*(v-ls)))) for v in Wv), A.mpf(0))
                    return n/kk+sum(((v-ls)*(1-2/(1+A.exp(-(kk*(v-ls))))) for v in Wv), A.mpf(0))
                out.append(bracket('root', 'dl/dk(k e^-w) > 0 > dl/dk(k e^w) at the held scale, w = 4 eps (2 eps bracket half width + rounding of exp and of the public power)',
                                   make_r, -1, 'numerical', CLAIMS['location']['scale held'][1], CLAIMS['location']['scale held'][0], numeric=numeric,
                                   u0=mp.log(mp.mpf(shape))))
            else:
                def make_r(A):
                    Wv = W(A); n = A.mpf(len(Wv)); k = A.mpf(shape); ls0 = A.log(A.mpf(scale)); sw = sum(Wv, A.mpf(0))
                    def g(kk):
                        if kind_name == 'invweibull':
                            e = [A.exp(-kk*v) for v in Wv]
                            return n/kk+n*sum((v*q for v, q in zip(Wv, e)), A.mpf(0))/sum(e, A.mpf(0))-sw
                        f = lambda c: -n+2*sum((1/(1+A.exp(-(kk*v-c))) for v in Wv), A.mpf(0))
                        c = decreasing_root(f, sum((kk*v for v in Wv), A.mpf(0))/n)
                        return n/kk+sw-2*sum((v/(1+A.exp(-(kk*v-c))) for v in Wv), A.mpf(0))
                    return g(k*A.exp(-A.mpf(w))), g(k*A.exp(A.mpf(w)))
                role = 'info'
                out.append(bracket('root', 'profile score dl/dk(k e^-w) > 0 > dl/dk(k e^w), w = 4 eps; information (the certificate is restored())',
                                   make_r, -1, 'numerical', CLAIMS['location']['free scale'][1], '4 eps absolute in log b (half width 2 eps)', role=role))
        return out, None
    return replay

def data_plain(shape_name, scale_name, bits):
    return lambda r, data: ([float(v) for v in data], shape_name, scale_name, bits)

def data_minus(r, data):   # pareto3: x - min, exact in the replay (min held)
    mn = float(r['held']['min'])
    return ([mp.mpf(float(v))-mp.mpf(mn) for v in data], 'shape', 'scale', 0)


# ------------------------------------------------------------ logis, dnorm, dweibull (Newton / Brent stops)

CLAIMS['logis'] = {
    'free': ('Newton: undamped step at most tol = 1e-10 in mu / max(|mu|, s) and log scale, applied (after standardization, relative to sd); no bracket', 'logis/mle.hpp:205-216 (full <= tol, accepted undamped Newton step)'),
    'location held': ('log scale bracket 1e-12, half width 5e-13', 'logis/mle.hpp:259 (hi - lo < 1e-12)'),
    'scale held': ('mu bracket 1e-12 s, half width 5e-13 s (adjacent doubles: a few spacings)', 'logis/mle.hpp:240 (hi - lo <= 1e-12 s)'),
}

def replay_logis(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate': return [], None
    x = [float(v) for v in data]; mu = estimate_of(r, 'location'); s = estimate_of(r, 'scale'); abstol = TOL['logis'][0]
    mask = r['mask']
    if mask == 3: return [], 'evaluation only'
    if mask == 0:
        def grad(A):
            X = [A.mpf(v) for v in x]; n = len(X)
            def g(m, th):
                sc = mp.exp(th); out_m = out_t = 0
                for v in X:
                    z = (v-m)/sc; P = 1/(1+mp.exp(-z))
                    out_m += (2*P-1)/sc; out_t += z*(2*P-1)
                return [out_m, out_t-n]
            return g
        def make(A):
            g = grad(A); t = newton_point(g, (mu, mp.log(mp.mpf(s))), hs=(mp.mpf(s)*mp.mpf(10)**(-mp.mp.dps//3), mp.mpf(10)**(-mp.mp.dps//3)))
            return abs(mp.mpf(mu)-t[0])/max(abs(mp.mpf(mu)), mp.exp(t[1])), abs(mp.log(mp.mpf(s))-t[1]), abs(mp.mpf(mu)-t[0])/mp.exp(t[1]), abs(mp.expm1(mp.log(mp.mpf(s))-t[1]))
        values = enclose(make, 'numerical', 50)
        tol_mu = abstol + math.ulp(mu)/2/max(abs(mu), s); tol_t = abstol + EPS   # rounding of the public location (half ulp) and scale
        ok = mp.mpf(values[0].b) <= tol_mu and mp.mpf(values[1].b) <= tol_t
        bad = mp.mpf(values[0].a) > tol_mu or mp.mpf(values[1].a) > tol_t
        return [comparison('root', '|mu - mu*| <= tol max(|mu*|, s*) + half ulp(mu) and |log s - log s*| <= tol + eps (Newton stop, tol)', 'holds' if ok else 'fails' if bad else 'open',
                           'mu offset/max(|mu*|,s*) %s, log s offset %s' % (mp.nstr(mid(values[0]), 3), mp.nstr(mid(values[1]), 3)), rigor='numerical',
                           note='hp Newton from the returned point; mu offset in scale units %s, scale relative offset %s; %s (%s)' % (
                               mp.nstr(mid(values[2]), 3), mp.nstr(mid(values[3]), 3), CLAIMS['logis']['free'][0], CLAIMS['logis']['free'][1]))], None
    if mask == 1:   # location held, scale root in t = log s
        m = float(r['held']['location']); t0 = math.log(s); w = 0.5e-12+EPS
        def score(A, X, t):
            sc = A.exp(t)
            tot = A.mpf(0)
            for v in X:
                z = (v-A.mpf(m))/sc
                tot += z*A.expm1(z)/(A.exp(z)+1)
            return tot-len(X)
        def make(A):
            X = [A.mpf(v) for v in x]; t = A.log(A.mpf(s)); return score(A, X, t-A.mpf(w)), score(A, X, t+A.mpf(w))
        def numeric(u):
            return score(MPOps, [mp.mpf(v) for v in x], u)
        return [bracket('root', 'h(log s - w) > 0 > h(log s + w), h = -n + sum z tanh(z/2), z = (x - mu)/s, w = 0.5e-12 + rounding', make, -1, 'interval',
                        CLAIMS['logis']['location held'][1], CLAIMS['logis']['location held'][0], numeric=numeric, u0=mp.log(mp.mpf(s)))], None
    if mask == 2:   # scale held, location root
        sc = float(r['held']['scale']); w = 0.5e-12*sc+2*EPS*abs(mu)
        def score(A, X, m):
            return sum((A.expm1((v-m)/sc)/(A.exp((v-m)/sc)+1) for v in X), A.mpf(0))
        def make(A):
            X = [A.mpf(v) for v in x]; m = A.mpf(mu); return score(A, X, m-A.mpf(w)), score(A, X, m+A.mpf(w))
        def numeric(u): return score(MPOps, [mp.mpf(v) for v in x], u)
        return [bracket('root', 'g(mu - w) > 0 > g(mu + w), g = sum tanh((x - mu)/(2 s)), w = 0.5e-12 s + 2 eps |mu|', make, -1, 'interval',
                        CLAIMS['logis']['scale held'][1], CLAIMS['logis']['scale held'][0], numeric=numeric, u0=mp.mpf(mu))], None
    return [], None

CLAIMS['dnorm'] = {'free or one held': ('Newton decrement g^T H^-1 g <= tol^2 n (tol = 1e-10) at the stopping iterate, after which the full Newton step is applied (so the decrement at the returned point is at most tol^2 n), or total score norm below 1e-6 n, in the coordinates A = (mu - c)/sd, B = 1/sd',
                                        'dnorm/mle.hpp:127 (converged = -slope <= tol tol n), :143-145 (full step applied, then stop), :150 (sqrt(norm) < 1e-6 n)')}

def replay_dnorm(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate': return [], None
    mask = r['mask']
    if mask == 3: return [], 'evaluation only'
    x = sorted(float(v) for v in data); n = len(x); tol = TOL['dnorm'][0]
    cells = collections.Counter(x); xs = sorted(cells); w = [float(cells[v]) for v in xs]
    offset = xs[0]; centre = 0.0
    for xv, wv in zip(xs, w): centre += wv*(xv-offset)/n
    centre += offset+0.5
    mu_hat = estimate_of(r, 'mean'); sd_hat = estimate_of(r, 'sd')
    held_mean = bool(mask & 1); held_sd = bool(mask & 2)
    def Pcell(a, b):
        s2 = mp.sqrt(2)
        return (mp.erfc(a/s2)-mp.erfc(b/s2))/2 if a+b > 0 else (mp.erfc(-b/s2)-mp.erfc(-a/s2))/2
    def nll(Av, Bv):
        mu = mp.mpf(centre)+Av/Bv; sg = 1/Bv
        return -sum(mp.mpf(wv)*mp.log(Pcell((mp.mpf(xv)-mu)/sg, (mp.mpf(xv)+1-mu)/sg)) for xv, wv in zip(xs, w))
    A0 = (mp.mpf(mu_hat)-mp.mpf(centre))/mp.mpf(sd_hat); B0 = 1/mp.mpf(sd_hat)
    def make(Ops):
        A_ = (mp.mpf(mu_hat)-mp.mpf(centre))/mp.mpf(sd_hat); B_ = 1/mp.mpf(sd_hat)
        if held_mean:
            held = mp.mpf(mu_hat)-mp.mpf(centre); f = lambda b: nll(held*b, b); th = [B_]
        elif held_sd:
            f = lambda a: nll(a, B_); th = [A_]
        else:
            f = nll; th = [A_, B_]
        g = numgrad(f, th); H = numhess(f, th)
        if len(th) == 1:
            dec = g[0]*g[0]/H[0][0]
        else:
            Hm = mp.matrix(H); sol = mp.lu_solve(Hm, mp.matrix(g)); dec = g[0]*sol[0]+g[1]*sol[1]
        gn = mp.sqrt(sum(v*v for v in g))
        return dec/(mp.mpf(tol)**2*n), gn/(mp.mpf(1e-6)*n)
    values = enclose(make, 'numerical', 50)
    ok = mp.mpf(values[0].b) <= 1+1e-4 or mp.mpf(values[1].b) < 1
    bad = mp.mpf(values[0].a) > 1+1e-4 and mp.mpf(values[1].a) >= 1
    return [comparison('root', 'Newton decrement g^T H^-1 g <= tol^2 n (tol 1e-10) at the returned point (allowance 1e-4 of the threshold), or ||g|| < 1e-6 n',
                       'holds' if ok else 'fails' if bad else 'open', 'decrement/(tol^2 n) = %s, ||g||/(1e-6 n) = %s' % (show(values[0], 5), show(values[1], 5)),
                       rigor='numerical', note='hp gradient and Hessian by central differences at 50 digits, coordinates A = (mu - c)/sd, B = 1/sd; %s (%s)' % (
                           CLAIMS['dnorm']['free or one held'][0], CLAIMS['dnorm']['free or one held'][1]))], None

CLAIMS['dweibull'] = {
    'beta (shape2) root': ('log shape: Brent stops at |c - b|/2 <= 2 eps |u| + tol/2, so |u - u*| <= 4 eps |u| + tol, tol = 1e-12', 'dweibull/mle.hpp:284-285 (brent_root tol1 = 2 eps |b| + tol/2)'),
    'alpha root (shape2 held)': ('Newton/bisection step <= 1e-13 (1 + |a|), a = alpha + beta log c, c the latent median plus half', 'dweibull/mle.hpp:258 (std::abs(step_now) <= 1e-13 (1 + |x|))'),
}

def dw_terms(A, counts, beta, t):
    """sum_k c_k [-t k^beta + log(-expm1(-t D_k))], D_k = (k+1)^beta - k^beta (dossier log_density in t = -log q)."""
    tot = A.mpf(0)
    for k, c in counts.items():
        kb = A.mpf(0) if k == 0 else A.exp(beta*A.log(A.mpf(k)))
        kb1 = A.exp(beta*A.log(A.mpf(k+1)))
        tot += c*(-t*kb+A.log(-A.expm1(-t*(kb1-kb))))
    return tot

def replay_dweibull(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate': return [], None
    mask = r['mask']
    if mask == 3: return [], 'evaluation only'
    x = [int(v) for v in data]; counts = collections.Counter(x)
    q = estimate_of(r, 'shape1'); beta = estimate_of(r, 'shape2'); tol = TOL['dweibull'][0]
    if max(counts) > 20000: return [], 'open: count range above 20000 (hp cost)'
    t_hat = -math.log(q); u_hat = math.log(beta)
    def d_beta(A, b, t):   # d/dbeta of the log likelihood at fixed t
        tot = A.mpf(0)
        for k, c in counts.items():
            kb = A.mpf(0) if k == 0 else A.exp(b*A.log(A.mpf(k)))
            lk = A.mpf(0) if k == 0 else A.log(A.mpf(k))
            kb1 = A.exp(b*A.log(A.mpf(k+1))); lk1 = A.log(A.mpf(k+1))
            D = kb1-kb; Dp = kb1*lk1-(kb*lk if k else A.mpf(0))
            tot += c*(-t*(kb*lk if k else A.mpf(0))+t*Dp/A.expm1(t*D))
        return tot
    if mask == 1:   # q held: beta root
        w = 4*EPS*abs(u_hat)+tol+EPS
        def make(A):
            t = -A.log(A.mpf(q)); b = A.mpf(beta)
            return d_beta(A, b*A.exp(-A.mpf(w)), t), d_beta(A, b*A.exp(A.mpf(w)), t)
        def numeric(u): return d_beta(MPOps, mp.exp(u), -mp.log(mp.mpf(q)))
        return [bracket('root', 'l_beta(beta e^-w) > 0 > l_beta(beta e^w) at the held q, w = 4 eps |log beta| + 1e-12 + rounding', make, -1, 'interval',
                        CLAIMS['dweibull']['beta (shape2) root'][1], CLAIMS['dweibull']['beta (shape2) root'][0], numeric=numeric, u0=mp.mpf(u_hat))], None
    def d_alpha(A, b, al):   # d/d alpha, alpha = log t
        t = A.exp(al); tot = A.mpf(0)
        for k, c in counts.items():
            kb = A.mpf(0) if k == 0 else A.exp(b*A.log(A.mpf(k)))
            kb1 = A.exp(b*A.log(A.mpf(k+1))); D = kb1-kb
            tot += c*(-t*kb+t*D/A.expm1(t*D))
        return tot
    if mask == 2:   # beta held: alpha root
        # a = alpha + beta log c with c = latent median plus one half (an exact count rule)
        srt = sorted(x); nn = len(srt); cum = 0; med = None
        n0 = counts.get(0, 0)
        if n0 >= nn/2: med = 0.5
        else:
            cum = n0
            for v in sorted(k for k in counts if k > 0):
                cum += counts[v]
                if cum >= nn/2: med = v+0.5; break
            if med is None: med = max(counts)+0.5
        a_hat = math.log(t_hat)
        wa = 1e-13*(1+abs(a_hat+beta*math.log(med)))+2*HALF/t_hat+EPS
        def make(A):
            b = A.mpf(beta); al = A.log(-A.log(A.mpf(q))); wv = A.mpf(wa)
            return d_alpha(A, b, al-wv), d_alpha(A, b, al+wv)
        def numeric(u): return d_alpha(MPOps, mp.mpf(beta), u)
        return [bracket('root', 'l_alpha(alpha - w) > 0 > l_alpha(alpha + w) at the held beta, w = 1e-13 (1 + |a|) + half-ulp(q)/t + rounding', make, -1, 'interval',
                        CLAIMS['dweibull']['alpha root (shape2 held)'][1], CLAIMS['dweibull']['alpha root (shape2 held)'][0], numeric=numeric, u0=mp.mpf(a_hat))], None
    # both free: 2D displacement in (alpha, log beta); the t < 1e-6 refit path is the held-q beta root
    if t_hat < 1e-6:
        w = 4*EPS*abs(u_hat)+tol+EPS
        def make(A):
            t = -A.log(A.mpf(q)); b = A.mpf(beta)
            return d_beta(A, b*A.exp(-A.mpf(w)), t), d_beta(A, b*A.exp(A.mpf(w)), t)
        def numeric(u): return d_beta(MPOps, mp.exp(u), -mp.log(mp.mpf(q)))
        return [bracket('root', 'refit path (t < 1e-6): l_beta(beta e^-w) > 0 > l_beta(beta e^w) at the returned q, w = 4 eps |log beta| + 1e-12 + rounding', make, -1, 'interval',
                        'dweibull/mle.hpp:368-371 (refit with q held), :284', CLAIMS['dweibull']['beta (shape2) root'][0], numeric=numeric, u0=mp.mpf(u_hat))], None
    def make(A):
        a0 = mp.log(mp.mpf(t_hat)); u0_ = mp.mpf(u_hat)
        def grad(al, ub):
            b = mp.exp(ub); t = mp.exp(al)
            return [d_alpha(MPOps, b, al), d_beta(MPOps, b, t)*b]
        sol = newton_point(grad, (-mp.log(mp.mpf(q)) and mp.log(-mp.log(mp.mpf(q))), u0_))
        return abs(sol[0]-mp.log(-mp.log(mp.mpf(q)))), abs(sol[1]-u0_)
    values = enclose(make, 'numerical', 50)
    wa = 1e-12*(1+abs(math.log(t_hat)))+2*HALF/t_hat+EPS; wb = 4*EPS*abs(u_hat)+tol+EPS
    ok = mp.mpf(values[0].b) <= wa and mp.mpf(values[1].b) <= wb
    bad = mp.mpf(values[0].a) > wa or mp.mpf(values[1].a) > wb
    return [comparison('root', '|alpha - alpha*| <= 1e-12 (1+|alpha|) + half-ulp(q)/t and |log beta - log beta*| <= 4 eps |log beta| + 1e-12', 'holds' if ok else 'fails' if bad else 'open',
                       'alpha offset %s (allowed %s), log beta offset %s (allowed %s)' % (show(values[0], 4), mp.nstr(wa, 3), show(values[1], 4), mp.nstr(wb, 3)),
                       rigor='numerical', note='hp Newton in (alpha, log beta) from the returned point; beta Brent stop %s; alpha inner stop %s' % (
                           CLAIMS['dweibull']['beta (shape2) root'][1], CLAIMS['dweibull']['alpha root (shape2 held)'][1]))], None


# ------------------------------------------------------------ zeta, pig

CLAIMS['zeta'] = {'free': ('bisection: stops at |score| <= tol mean log x or hi - lo <= tol (s - 1), tol = 1e-10; s is an end of the bracket in the second case', 'zeta/mle.hpp:158')}

def replay_zeta(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate' or not free_flag(r, 0): return [], 'no root comparison'
    x = [int(v) for v in data]; n = len(x); s_hat = estimate_of(r, 's'); tol = TOL['zeta'][0]
    counts = collections.Counter(x)
    def make(A):
        tgt = sum((c*A.log(A.mpf(k)) for k, c in counts.items()), A.mpf(0))/n
        S = lambda s: -mp.zeta(s, 1, 1)/mp.zeta(s)-tgt
        sh = A.mpf(s_hat); w = A.mpf(tol)*(sh-1)+A.mpf(EPS)*sh
        return S(sh-w), S(sh+w), S(sh), tgt
    values = enclose(make, 'numerical', 50)
    br_ok = mp.mpf(values[0].a) > 0 and mp.mpf(values[1].b) < 0
    br_bad = mp.mpf(values[0].b) < 0 or mp.mpf(values[1].a) > 0
    res = abs(values[2]); res_ok = mp.mpf(res.b) <= tol*mp.mpf(values[3].a); res_bad = mp.mpf(res.a) > tol*mp.mpf(values[3].b)
    verdict = 'holds' if (br_ok or res_ok) else 'fails' if (br_bad and res_bad) else 'open'
    return [comparison('root', 'score(s - w) > 0 > score(s + w), w = tol (s - 1) + rounding, OR |score(s)| <= tol mean log x (either stopping rule)', verdict,
                       'S(s-w) %s, S(s+w) %s, |S(s)|/target %s' % (show(values[0], 5), show(values[1], 5), mp.nstr(mp.mpf(res.b)/mp.mpf(values[3].a), 3)), rigor='numerical',
                       note='bracket rule %s, residual rule %s; %s (%s)' % ('holds' if br_ok else 'fails' if br_bad else 'open', 'holds' if res_ok else 'fails' if res_bad else 'open',
                                                                         CLAIMS['zeta']['free'][0], CLAIMS['zeta']['free'][1]))], None

CLAIMS['pig'] = {
    'shape held (mean root)': ('log mean bracket tol = 1e-10, half width 5e-11 (padded endpoint signs); the padded fallback shrinks to 0.49 tol on each side', 'pig/mle.hpp:119,127 (hi-lo <= tol)'),
    'shape free': ('log shape enclosure 2e-7 (padded signs), polished to .01 tol = 1e-12 inside it', 'pig/mle.hpp:313-315 (resolution = max(tol, 2e-7)), :323 (b-a <= .01 tol)'),
}

def pig_ll(A, counts, mean, shape):
    n_total = 0
    z = A.sqrt(shape*(2+shape/(mean*mean)))
    base = A.log(2*shape/A.mpf(mp.pi))/2+shape/mean+A.log(shape/(2+shape/(mean*mean)))/4*0
    tot = A.mpf(0)
    for k, c in counts.items():
        nn = max(0, k-1); coef = A.mpf(1); poly = A.mpf(1); two_z = 2*z; term = A.mpf(1)
        for j in range(1, nn+1):
            coef = coef*(nn+j)*(nn-j+1)/j
            poly += coef/two_z**j
        lk = A.log(A.sqrt(A.mpf(mp.pi)/(2*z)))-z+A.log(poly)
        lp = A.log(2*shape/A.mpf(mp.pi))/2-A.loggamma(k+1)+shape/mean+(k-A.mpf(1)/2)*A.log(shape/(2+shape/(mean*mean)))/2+lk
        tot += c*lp
    return tot

def replay_pig(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate': return [], None
    mask = r['mask']
    if mask == 3: return [], 'evaluation only'
    x = [int(v) for v in data]; counts = collections.Counter(x)
    if max(counts) > 1500: return [], 'open: count range above 1500 (hp polynomial cost); %d cases of this kind' % 1
    mean = estimate_of(r, 'mean'); shape = estimate_of(r, 'shape'); tol = TOL['pig'][0]
    if not (shape > 0 and math.isfinite(shape)) or max(counts) <= 1 and mask & 1: return [], 'binary counts: closed form (exact predicate)'
    h = lambda: mp.mpf(10)**(-mp.mp.dps//3)
    if mask == 2:   # shape held: mean root in log mean
        w = tol/2+EPS
        def sc(A, lm):
            hh = h(); f = lambda v: pig_ll(A, counts, mp.exp(v), mp.mpf(shape))
            return (f(lm+hh)-f(lm-hh))/(2*hh)
        def make(A):
            lm = mp.log(mp.mpf(mean)); return sc(A, lm-mp.mpf(w)), sc(A, lm+mp.mpf(w))
        def numeric(u): return sc(MPOps, u)
        return [bracket('root', 'l(log mu - w) > 0 > l(log mu + w) at the held shape (derivative of the dossier likelihood), w = tol/2 + rounding', make, -1, 'numerical',
                        CLAIMS['pig']['shape held (mean root)'][1], CLAIMS['pig']['shape held (mean root)'][0], numeric=numeric, u0=mp.log(mp.mpf(mean)))], None
    # shape free (mean free: xbar, or held)
    def sc(A, ls):
        hh = h(); f = lambda v: pig_ll(A, counts, mp.mpf(mean), mp.exp(v))
        return (f(ls+hh)-f(ls-hh))/(2*hh)
    out = []
    for name, w, role, key in (('root', 2e-7+EPS, 'judged', 'shape free'), ('root-polish', 0.5e-12+EPS, 'info', 'shape free')):
        def make(A, w=w):
            ls = mp.log(mp.mpf(shape)); return sc(A, ls-mp.mpf(w)), sc(A, ls+mp.mpf(w))
        def numeric(u): return sc(MPOps, u)
        out.append(bracket(name, 'l(log lambda - w) > 0 > l(log lambda + w) at the returned mean, w = %g + rounding' % w, make, -1, 'numerical', CLAIMS['pig'][key][1],
                           CLAIMS['pig'][key][0], role=role, numeric=numeric if name == 'root-polish' else None, u0=mp.log(mp.mpf(shape))))
    return out, None


# ------------------------------------------------------------ genbeta (both beta shapes held; one shape held with power held), frechet, gev

CLAIMS['genbeta'] = {
    'a, b, scale held': ('padded opposite signs of t L\'(t)/n at t(1 -/+ 2.5e-11)', 'genbeta/mle.hpp:283-292 (radius = min(tol, 1e-10) .25)'),
    'power and one shape held': ('relative bracket tol/8 = 1.25e-11 in the free shape, half width 6.25e-12', 'genbeta/mle.hpp:476-482 (hi - lo <= tol .125 lo)'),
    'power and scale held': ('beta_mle on (x/scale)^power, tol = 1e-10', 'genbeta/mle.hpp:464-466; beta/mle.hpp'),
}

def replay_genbeta(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate': return [], None
    mask = r['mask']; x = [float(v) for v in data]
    scale = float(r['held']['scale']) if 'scale' in r['held'] else None
    if scale is None: return [], 'scale free: cover (not a scalar root)'
    tol = TOL['genbeta'][0]
    if mask == 11:   # shape1, shape2, scale held: power root
        a = float(r['held']['shape1']); b = float(r['held']['shape2']); t_hat = estimate_of(r, 'shape3'); w = 0.25*min(tol, 1e-10)
        def S(A, Y, tt):
            n = len(Y); z = [tt*v for v in Y]
            ybar = sum(Y, A.mpf(0))/n
            return 1-A.mpf(a)*tt*ybar+(A.mpf(b)-1)*sum((v/A.expm1(v) for v in z), A.mpf(0))/n
        def make(A):
            Y = [-A.log(A.mpf(v)/A.mpf(scale)) for v in x]; t = A.mpf(t_hat)
            return S(A, Y, t*(1-A.mpf(w))), S(A, Y, t*(1+A.mpf(w)))
        def numeric(u):
            Y = [-mp.log(mp.mpf(v)/mp.mpf(scale)) for v in x]; return S(MPOps, Y, mp.exp(u))
        return [bracket('root', "S(t(1-w)) > 0 > S(t(1+w)), S = 1 - a t mean y + (b-1) mean z/expm1(z), y = -log(x/scale), z = t y, w = 2.5e-11 (signs only; the core also requires them to exceed its pad)",
                        make, -1, 'interval', CLAIMS['genbeta']['a, b, scale held'][1], CLAIMS['genbeta']['a, b, scale held'][0], numeric=numeric, u0=mp.log(mp.mpf(t_hat)))], None
    if mask in (12, 13, 14):   # power and scale held
        t = float(r['held']['shape3'])
        xs = lambda A: [A.exp(A.mpf(t)*A.log(A.mpf(v)/A.mpf(scale))) for v in x]
        names = ('shape1', 'shape2')
        if mask == 12:
            return [beta_free_roots(xs, 0, r, names, tol, 'genbeta power and scale held: beta on (x/scale)^power', CLAIMS['genbeta']['power and scale held'][1])], None
        held_name = 'shape1' if mask == 13 else 'shape2'
        return [beta_held_root(xs, r, names, held_name, tol/16, 'genbeta power, scale and %s held' % held_name, CLAIMS['genbeta']['power and one shape held'][1],
                               CLAIMS['genbeta']['power and one shape held'][0])], None
    return [], 'not a scalar root (cover or evaluation)'

CLAIMS['frechet'] = {
    'lambda held': ('bracket in u = log(-mu_std): hi - lo <= tol (1 + |mid|), half width tol/2 (1 + |u|), plus the rounding of the public mu converted to u', 'frechet/certificate.hpp:60 (hi - lo <= tol (1 + |mid|)), :61 finish((lo+hi)/2)'),
}

def replay_frechet(r, data, budget):
    kind, certified = kind_of(r)
    if kind != 'estimate': return [], None
    mask = r['mask']; x = [float(v) for v in data]; tol = TOL['frechet'][0]
    lam_held = bool(mask & 1); mu_held = bool(mask & 2); sig_held = bool(mask & 4)
    if mu_held:
        if lam_held and sig_held: return [], 'evaluation only'
        mu = float(r['held']['mu'])
        pseudo = dict(r); pseudo['mask'] = (1 if lam_held else 0) | (2 if sig_held else 0)
        pseudo['held'] = {}
        if lam_held: pseudo['held']['shape'] = r['held']['lambda']
        if sig_held: pseudo['held']['scale'] = r['held']['sigma']
        pseudo['result'] = dict(r['result']); pseudo['result']['estimates'] = {'shape': r['result']['estimates']['lambda'], 'scale': r['result']['estimates']['sigma']}
        fn = replay_location('invweibull', 'frechet', lambda rr, d: ([mp.mpf(float(v))-mp.mpf(mu) for v in d], 'shape', 'scale', 0), tol, 'bindings/common/distributions/frechet/fit.hpp:5 reltol')
        comps, why = fn(pseudo, data, budget)
        for c in comps: c['note'] = 'invweibull on x - mu (mu held); '+c['note']
        return comps, why
    if lam_held and not mu_held:
        lam = float(r['held']['lambda']); mu_hat = estimate_of(r, 'mu'); sigma_hat = estimate_of(r, 'sigma')
        m = min(x); M = max(x); unit = sigma_hat if sig_held else M-m
        if not (unit > 0 and mu_hat < m): return [], 'open: no valid standardization (unit %r)' % unit
        u0 = math.log((m-mu_hat)/unit)
        w = 0.5*tol*(1+abs(u0))+2*HALF*abs(mu_hat)/(m-mu_hat)
        def score(A, Y, u):
            e = A.exp(u); n = len(Y); a = A.mpf(lam)
            rr = [e/(v+e) for v in Y]; p = [A.exp(a*A.log(v)) for v in rr]
            Aa = sum((pi*ri for pi, ri in zip(p, rr)), A.mpf(0)); Bb = sum(p, A.mpf(0)); Cc = sum(rr, A.mpf(0))
            base = (-a*u+A.log(Aa/Cc)) if sig_held else (A.log(A.mpf(n)/Cc)+A.log(Aa/Bb))
            return base-A.log1p(1/a)
        def make(A):
            Y = [(A.mpf(v)-A.mpf(m))/A.mpf(unit) for v in x]; u = A.log((A.mpf(m)-A.mpf(mu_hat))/A.mpf(unit)); wv = A.mpf(w)
            return score(A, Y, u-wv), score(A, Y, u+wv)
        def numeric(uu):
            Y = [(mp.mpf(v)-mp.mpf(m))/mp.mpf(unit) for v in x]; return score(MPOps, Y, uu)
        return [bracket('root', 'score(u - w) > 0 > score(u + w), u = log(-mu_std) of the standardized location, w = tol/2 (1+|u|) + rounding of mu', make, -1, 'interval',
                        CLAIMS['frechet']['lambda held'][1], CLAIMS['frechet']['lambda held'][0], numeric=numeric, u0=mp.mpf(u0))], None
    return [], 'not a scalar root of this job (held scale with free shape: cover; all free: cover)'

CLAIMS['gev'] = {'sigma, xi held, -1 < xi < 0': ('mu bracket tol sigma (standardized: hi - lo <= tol), half width tol sigma/2; |score| < 1e-3 at the end point', 'gev/mle.hpp:279-290 (gev_held_profile, hi - lo <= tol sigma)')}

def replay_gev(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate': return [], None
    if r['mask'] != 6: return [], 'not a held-scale scalar root (subset handled elsewhere)'
    sigma = float(r['held']['sigma']); xi = float(r['held']['xi'])
    if not (-1 < xi < 0): return [], 'open: held-scale root with xi %g outside (-1, 0): golden-section search (gev/mle.hpp:292-300), no bracket claim' % xi
    mu_hat = estimate_of(r, 'mu'); x = [float(v) for v in data]; tol = TOL['gev'][0]
    w = 0.5*tol*sigma+4*HALF*abs(mu_hat)
    def score(A, X, m):
        tot = A.mpf(0)
        for v in X:
            tt = 1+A.mpf(xi)*(v-m)/A.mpf(sigma)
            tot += (A.mpf(xi)+1-A.exp(-A.log(tt)/A.mpf(xi)))/tt
        return tot
    def make(A):
        X = [A.mpf(v) for v in x]; m = A.mpf(mu_hat); return score(A, X, m-A.mpf(w)), score(A, X, m+A.mpf(w))
    def numeric(u): return score(MPOps, [mp.mpf(v) for v in x], u)
    return [bracket('root', 'S(mu - w) > 0 > S(mu + w), S = sum (xi + 1 - t^(-1/xi))/t, t = 1 + xi (x - mu)/sigma, w = tol sigma/2 + rounding', make, -1, 'interval',
                    CLAIMS['gev']['sigma, xi held, -1 < xi < 0'][1], CLAIMS['gev']['sigma, xi held, -1 < xi < 0'][0], numeric=numeric, u0=mp.mpf(mu_hat))], None


# ------------------------------------------------------------ modifier routes: conditional-mean equations

CLAIMS['routes'] = {
    'ztpois, zmpois': ('no bracket: 100 bisection halvings of [0, mean]; claim taken as the accuracy contract |lambda - lambda*| <= 1e-10 max(1, lambda)', 'pois/ztpois_mle.hpp:19-22; accuracy-contract.md'),
    'zmlogarithmic': ('residual |M(theta) - mean| < 1e-10 mean (lgser path) or bracket 1e-12 (bisection path)', 'core/zero_modified.hpp:44-45,52'),
    'zipf': ('padded signs at s -/+ rel/(1+rel) max(1,|s|), rel = min(tol, 1) = 1e-10', 'zeta/truncation.hpp:140-156'),
}

def replay_ztpois_route(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate' or not free_flag(r, 0): return [], 'no root comparison'
    pos = [int(v) for v in data if int(v) > 0]
    if not pos: return [], 'all zero'
    lam = estimate_of(r, 'lambda'); m = Fraction(sum(pos), len(pos))
    if m == 1: return [], 'mean one: closed form'
    w = 1e-10*max(1.0, lam)
    def make(A):
        l = A.mpf(lam); mm = A.mpf(m.numerator)/A.mpf(m.denominator)
        f = lambda v: v/(-A.expm1(-v))-mm
        return f(l-A.mpf(w)), f(l+A.mpf(w))
    def numeric(u):
        mm = mp.mpf(m.numerator)/m.denominator; return u/(-mp.expm1(-u))-mm
    return [bracket('moment', 'M(lambda - w) < mean(positives) < M(lambda + w), M = lambda/(1 - e^-lambda), w = 1e-10 max(1, lambda)', make, +1, 'interval',
                    CLAIMS['routes']['ztpois, zmpois'][1], CLAIMS['routes']['ztpois, zmpois'][0], numeric=numeric, u0=mp.mpf(lam))], None

def replay_zmlogarithmic_route(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate': return [], None
    names = r['result']['estimates']
    key = 'theta' if 'theta' in names else 'prob'
    if (r['mask'] & 1): return [], 'prob held'
    pos = [int(v) for v in data if int(v) > 0]
    if not pos: return [], 'all zero'
    th = estimate_of(r, key); m = Fraction(sum(pos), len(pos))
    if m == 1: return [], 'mean one: closed form'
    w = 0.5e-12+math.ulp(th)/2   # bisection path: root bracket of width 1e-12 plus rounding of the public theta
    def make(A):
        mm = A.mpf(m.numerator)/A.mpf(m.denominator)
        M = lambda t: -t/((1-t)*A.log1p(-t))
        t = A.mpf(th)
        return abs(M(t)-mm)-A.mpf(1e-10)*mm, M(t-A.mpf(w))-mm, M(t+A.mpf(w))-mm
    def judge(v):
        ok = mp.mpf(v[0].b) < 0 or (mp.mpf(v[1].b) < 0 and mp.mpf(v[2].a) > 0)
        bad = mp.mpf(v[0].a) > 0 and (mp.mpf(v[1].a) > 0 or mp.mpf(v[2].b) < 0)
        return 'holds' if ok else 'fails' if bad else 'open'
    values, verdict, used = settle(make, judge, 'interval')
    verdict = judge(values)
    return [comparison('moment', '|M(theta) - mean(positives)| < 1e-10 mean (lgser path) OR M(theta - w) < mean < M(theta + w), w = 0.5e-12 + rounding (bisection path), M = -theta/((1-theta) log(1-theta))', verdict,
                       'residual - 1e-10 mean = %s; M(theta-w)-mean = %s; M(theta+w)-mean = %s' % (show(values[0], 5), show(values[1], 5), show(values[2], 5)),
                       rigor='interval', note='%s (%s)' % (CLAIMS['routes']['zmlogarithmic'][0], CLAIMS['routes']['zmlogarithmic'][1]))], None

def replay_zipf_route(r, data, budget):
    kind, _ = kind_of(r)
    if kind != 'estimate': return [], None
    est = r['result']['estimates']
    if 'N' not in est or not (r['mask'] & 1) or (r['mask'] & 2): return [], 'N free (plain zeta fit) or s held: no window root'
    N = int(float(r['held']['N'])); s_hat = estimate_of(r, 's'); x = [int(v) for v in data]
    if N > 4000: return [], 'open: window N = %d above 4000 (direct sum cost)' % N
    if all(v == 1 for v in x) or all(v == N for v in x): return [], 'no root comparison (limit)'
    rel = min(TOL['route'][0], 1.0); rad = rel/(1+rel)*max(1.0, abs(s_hat))
    counts = collections.Counter(x)
    def S(A, s, tgt):
        num = sum((A.log(A.mpf(k))*A.exp(-s*A.log(A.mpf(k))) for k in range(1, N+1)), A.mpf(0)); den = sum((A.exp(-s*A.log(A.mpf(k))) for k in range(1, N+1)), A.mpf(0))
        return num/den-tgt
    def make(A):
        tgt = sum((c*A.log(A.mpf(k)) for k, c in counts.items()), A.mpf(0))/len(x); s = A.mpf(s_hat); w = A.mpf(rad)
        return S(A, s-w, tgt), S(A, s+w, tgt)
    return [bracket('root', 'E_{s-w}[log K] > mean log x > E_{s+w}[log K], K ~ p(k) proportional to k^-s on 1..N, w = rel/(1+rel) max(1,|s|)', make, -1, 'interval',
                    CLAIMS['routes']['zipf'][1], CLAIMS['routes']['zipf'][0])], None

REPLAYS = {
    'gumbel': replay_gumbel, 'weibull': replay_weibull, 'kumar': replay_kumar, 'hcauchy': replay_hcauchy,
    'lgser': replay_lgser, 'yulesimon': replay_yulesimon, 'lnorm': replay_lnorm, 'logitnorm': replay_logitnorm,
    'beta': replay_beta, 'nsbeta': replay_nsbeta, 'gamma': replay_gamma,
    'invgamma': replay_gamma_raw('invgamma'), 'naka': replay_gamma_raw('naka'), 'lgamma': replay_gamma_raw('lgamma'),
    'invweibull': replay_location('invweibull', 'invweibull', data_plain('shape', 'scale', 0), TOL['invweibull'][0], TOL['invweibull'][1]),
    'llogis': replay_location('llogis', 'llogis', data_plain('shape', 'scale', 0), TOL['llogis'][0], TOL['llogis'][1]),
    'pareto3': replay_location('llogis', 'pareto3', data_minus, TOL['pareto3'][0], TOL['pareto3'][1], polish_width=TOL['pareto3'][0]),
    'logis': replay_logis, 'dnorm': replay_dnorm, 'dweibull': replay_dweibull, 'zeta': replay_zeta, 'pig': replay_pig,
    'genbeta': replay_genbeta, 'frechet': replay_frechet, 'gev': replay_gev,
    'pois+zero=truncated': replay_ztpois_route, 'pois+zero=modified': replay_ztpois_route,
    'lgser+zero=modified': replay_zmlogarithmic_route, 'zeta +truncate=zipf': replay_zipf_route,
}

def guarded(function, name):
    """An evaluation that raises (singular Jacobian, domain error, overflow) is OPEN with the reason, never silently skipped."""
    def replay(r, data, budget):
        try:
            comps, why = function(r, data, budget)
            if kind_of(r)[1] is False:   # an uncertified estimate makes no certificate claim: report, do not judge
                for c in comps:
                    if c['verdict'] in ('holds', 'fails', 'open'):
                        c['judged_verdict'] = c['verdict']; c['verdict'] = 'info'; c['note'] = 'outcome uncertified, not judged; '+c['note']
            return comps, why
        except (ZeroDivisionError, ValueError, OverflowError, ArithmeticError, TypeError) as error:
            comparison_name = 'restore' if name in ('lnorm', 'logitnorm', 'invgamma', 'naka', 'lgamma', 'llogis', 'invweibull', 'pareto3') and r['mask'] % 4 != 2 else 'root'
            return [comparison(comparison_name, 'evaluation raised', 'open', 'evaluation error: %s: %s' % (type(error).__name__, error), rigor='numerical',
                               note='the high-precision evaluation failed at the returned coefficients (typically unrepresentable or degenerate coefficients)')], None
    return replay

REPLAYS = {k: guarded(v, k) for k, v in REPLAYS.items()}

