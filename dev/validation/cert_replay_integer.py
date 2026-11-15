"""Certificate replay, mechanism G (exhaustive integer searches), step 8e, job replay-integer (TASK-214.35).

Registered into cert-replay.py's REPLAYS; that script owns the driver, the verdict rules and the strict-layer
ratchet (dev/design/certificate-replay.md). Every function has the signature replay(record, data, budget) ->
(comparisons, why_none) of the prototype. Report: dev/research/evaluations/2026-11-15-replay-integer-report.md.

Each replay reads the claim from the snapshot record (outcome kind, certified, unique, returned coefficients,
the mask of held coefficients), states what the family README and mle.hpp say that claim rests on (an enumerated
integer range, an adjacent-ratio sign change, a tail bound), and evaluates the profile likelihood at integer
values and the tail bound in high precision. Nothing is read from the core beyond the record. Code read at master
b80a1831 (the baselines are the post-5b snapshots); a change to one of these searches must update this module in
the same commit. Source lines are in CLAIMS below and in the report.

Evidence levels (the `rigor` field of a comparison):

  exact      Python integers and fractions.Fraction: every double is a dyadic rational, and the count likelihood
             ratios used here are rational functions of integers (a power of an integer ratio per distinct count).
  interval   mpmath iv at 50 digits and up (outward rounded), retried at 100, 200, 400 digits while an enclosure
             contains the threshold; log of exact integer binomials or of exact rationals, no gamma function.
  numerical  ordinary mpmath at 50 digits (a profile over a continuous coefficient found by Newton's method on
             its score, then evaluated): evidence, not proof.
  screened   binary64 values with a margin of 1e-6(1 + |l|) used to discard grid points before the candidates are
             judged at the interval level; used only for grids above SCREEN_ABOVE points.

Comparisons (check names cert-replay:<name>):

  search     the likelihood at the returned integer(s) is at least the likelihood at every enumerated integer of the
             range the certificate covers (profile over any free continuous coefficient); one-coordinate searches
             check the adjacent-ratio sign pattern at every integer of the range when it is short, and always at
             the returned integer and its left neighbour
  tail       the tail bound that closes the search beyond the enumerated range, evaluated at the first integer
             where the replay's own cutoff closes it, and the bound is below the returned likelihood
  tie        the certified estimate is flagged unique: no other point of the model attains the same likelihood
             (exact tie test on ratios; a flagged tie against an exact gap is `open`, with the gap)
  coef       a continuous coefficient returned next to the integer is the profile maximizer at it (binom prob)

binom      mask 0 (size free)   search: D(k) = l(k+1) - l(k) of the profile at p = xbar/k changes sign at the
                                returned size k (D(k-1) > 0 >= D(k)) and the profile is unimodal on the enumerated
                                range; tail: D < 0 at k + 2^j (sampled; DeRiggi (1983) proves it)
                                [README Details; mle.hpp:225-304 (`increment`, doubling then bisection)]
           mask 2 (prob held)   search/tail: R(k) = L(k+1)/L(k) = (1-p)^n prod ((k+1)/(k+1-x_i)) exactly, R(k-1) > 1
                                >= R(k), R decreasing [README existence/reason; mle.hpp:104-121]
hyper      mask 6 / 3 (m or k)  search: the likelihood ratio of adjacent integers in the feasible range, exact
                                [mle.hpp:11-16, 285 `unimodal_peak`; increment_m]
           mask 5 (N free)      search: adjacent ratio in N exact; tail: u > k(m+k)/mean(x) makes D(u) < 0
                                [mle.hpp:18-25, increment_N, 669]
           mask 2 (N held)      search: every (m, k) of the grid {m, k >= max x, m + k <= N + min x}; tie: the
                                likelihood is symmetric in (m, k), so a returned m != k ties with (k, m)
                                [mle.hpp:562-605, README Integer grids]
           mask 1 / 4 (m or k)  search: every population N below the replay's own cutoff, profile over the free
                                count; tail: B(s) + h sum_{j<s} -log(1 - j/N) < l*, B(s) the binomial face
                                [README Growing-population bound; mle.hpp:464-561]
           mask 0               constant samples only (likelihood one): search = exact value one; tie: (N,N,c)
                                also attains one [README Details; mle.hpp:322-327]
nhyper     analogous, with the waiting-time likelihood; tails: adjacent population ratio below one beyond
           m max(x)/r, the shifted negative-binomial face bound B_r + sum_i -c_N(x_i) [README Growing-population
           bound; mle.hpp]
bbinom     mask 6 (both shapes held)  search: every size in [max x, N_dec + 1], N_dec = ceil(max x (alpha+beta-1)/alpha) - 1, from
                                the exact pmf; tail: alpha (N+1) >= x (alpha+beta-1) makes every pmf decrease from N_dec on
                                [mle.hpp:514-528, README Details]
           mask 4 (beta held)   tail: m (N+1-M) >= h M (beta-1) H_N exactly (m positive counts, H_N = sum_{j<=N} 1/(beta+j));
                                search: the alpha-profile at each size up to it, numerical [mle.hpp:465-512, README
                                Held-beta tail certificate]
           mask 2 (alpha held)  tail: the signed bounds B(N, c) and F + sum x(x+1)/2/(N-M) + h a^2/2 (1/(cN) + 1/(cN)^2) below
                                l* from a size on (interval, c from a grid); search: the beta-profile at each size below,
                                numerical [mle.hpp:402-463, README Held alpha: negative-binomial face and uniform size bound]
cmb        mask 6 (prob, nu held)  search: every size from max x to the first one where the decreasing bound is below l*; tail:
                                that bound (nu > 0: normalizer term at max x + 1; nu < 0: endpoint terms), interval
                                [mle.hpp:1406-1467, README Details]
           masks 0, 2, 4        search: the profile over the free coefficient(s) (a concave exponential family, damped Newton,
                                numerical) at every size from max x to max(N* + 8, max x + 40); tail: `info`, not replayed (the
                                K-truncated entropy-dual and block bounds of README Free-size profile bounds need an
                                optimization with no interval version); tie: at size one nu is not identified
triang     masks 0-3 (mode free) search: the profile over the free endpoints at every candidate mode (distinct observations
                                within the held endpoints), data standardized exactly: closed form and interval when both
                                endpoints are held (mask 3, exact rational ties), otherwise binary64 L-BFGS screening and a
                                40-digit root of the endpoint scores (numerical); tie: another mode within 1e-20
                                [mle.hpp:193-201, 207-280, 324; README Details]

Evidence in a record (`rigor`): the rigor of the comparison is the weakest level it uses; a grid comparison is `interval`
(candidates judged by intervals; exact Fractions decide overlaps) and states the number of points screened in binary64.
A `fails` on `tie` means: an exact (or, for triang and the numerical profiles, numerical) tie with the returned point
exists while the outcome carries unique = true.

Notation: n sample size, S = sum x, mx = max x, w_v the multiplicity of the value v.
"""
import collections, contextlib, functools, itertools, math
from fractions import Fraction
import mpmath as mp
from strict_common import number

iv = mp.iv
EPS = 2.0**-52
DPS_STEPS = (50, 100, 200, 400, 800)
SCREEN_ABOVE = 4000          # grid points above which binary64 screening precedes the interval judgement
CLAIMS = {}                  # family -> {subset: (claim text, source)}; reproduced in the report


# ---------------------------------------------------------------- arithmetic

def comparison(name, claim, verdict, hp, double=None, margin=None, rigor='interval', note='', info=None, **extra):
    return dict(name=name, claim=claim, verdict=verdict, hp=hp, double=double, margin=margin,
                rigor=rigor, note=note, info=info, **extra)

@contextlib.contextmanager
def precision(d):
    saved = (iv.dps, mp.mp.dps)
    iv.dps = d; mp.mp.dps = d
    try: yield
    finally: iv.dps, mp.mp.dps = saved

def I(q):
    """Interval from an integer or Fraction (outward rounded)."""
    if isinstance(q, Fraction): return iv.mpf(q.numerator)/iv.mpf(q.denominator) if q.denominator != 1 else iv.mpf(q.numerator)
    return iv.mpf(int(q)) if not isinstance(q, (float, mp.mpf)) else iv.mpf(q)

def ilog(q): return iv.log(I(q))

def sign(v):
    """+1 / -1 if the interval lies strictly on one side of zero, else 0."""
    lo, hi = mp.mpf(v.a), mp.mpf(v.b)
    return 1 if lo > 0 else -1 if hi < 0 else 0

def show(v, digits=15):
    if v is None: return None
    if isinstance(v, iv.mpf): return '[%s, %s]' % (mp.nstr(mp.mpf(v.a), digits), mp.nstr(mp.mpf(v.b), digits))
    if isinstance(v, (int, Fraction)) and not isinstance(v, bool): return str(v)
    if isinstance(v, float): return repr(v)
    return mp.nstr(v, digits)

@functools.lru_cache(maxsize=None)
def _lc(a, b, dps):
    return iv.log(iv.mpf(math.comb(a, b)))

def lc(a, b):
    """Interval of log C(a, b), exact integer binomial; None for an impossible binomial (probability zero)."""
    if b < 0 or a < 0 or b > a: return None
    if b > a-b: b = a-b
    return _lc(a, b, iv.dps)

def adaptive(make, decide, steps=DPS_STEPS):
    """make() at increasing precision until decide(value) is not None. Returns (value, decision, dps)."""
    value = decision = None; used = steps[0]
    for d in steps:
        with precision(d):
            value = make(); decision = decide(value); used = d
        if decision is not None: break
    return value, decision, used

def counts_of(data): return collections.Counter(int(v) for v in data)

def outcome_of(r):
    o = r['result'].get('outcome') or {}
    return o.get('kind'), bool(o.get('certified')), o.get('unique', True)

def estimates(r): return {k: number(v) for k, v in (r['result'].get('estimates') or {}).items()}

def fint(v):
    v = float(v)
    return int(v) if v.is_integer() else None

# ---------------------------------------------------------------- binom

CLAIMS['binom'] = {
    'size free': ('profile l(k) at p = xbar/k, smallest k >= max x with l(k+1) <= l(k); unimodal (DeRiggi 1983)',
                  'binom/mle.hpp:225-304 (increment, doubling, bisection), README Details'),
    'prob held': ('smallest k >= max x with L(k+1)/L(k) = (1-p)^n prod (k+1)/(k+1-x_i) <= 1; the ratio decreases in k',
                  'binom/mle.hpp:104-121, README existence reason'),
}
EXACT_POWER = 3000      # exact integer powers up to this exponent; interval beyond

def xlogx(m):
    return iv.mpf(0) if m == 0 else I(m)*ilog(m)

def binom_ent(S, n, k):
    """S log S + (nk - S) log(nk - S) - nk log(nk): the p = S/(nk) part of the profile log likelihood."""
    return xlogx(S)+xlogx(n*k-S)-xlogx(n*k)

def binom_D_iv(counts, S, n, k):
    """l(k+1) - l(k) of the free-size profile, as an interval."""
    d = iv.mpf(0)
    for v, w in counts.items(): d += w*ilog(Fraction(k+1, k+1-v))
    return d+binom_ent(S, n, k+1)-binom_ent(S, n, k)

def binom_R_exact(counts, S, n, k):
    """(numerator, denominator) integers of L(k+1)/L(k) for the free-size profile; None when the powers are large."""
    if n*(k+1) > EXACT_POWER: return None
    num = (n*(k+1)-S)**(n*(k+1)-S)*(n*k)**(n*k)
    den = (n*k-S)**(n*k-S)*(n*(k+1))**(n*(k+1))
    for v, w in counts.items():
        num *= (k+1)**w; den *= (k+1-v)**w
    return num, den

def binom_D_sign(counts, S, n, k):
    """(sign of l(k+1)-l(k) or None, rigor, enclosure text)."""
    exact = binom_R_exact(counts, S, n, k)
    if exact is not None:
        num, den = exact
        return (num > den)-(num < den), 'exact', '%s' % ('R > 1' if num > den else 'R < 1' if num < den else 'R = 1')
    value, s, _ = adaptive(lambda: binom_D_iv(counts, S, n, k), lambda v: sign(v) or None)
    return (s, 'interval', show(value, 8)) if s is not None else (None, 'interval', show(value, 8))

def held_R_sign(counts, n, p, k):
    """Exact sign of log R(k) for prob held: R(k) = (1-p)^n prod ((k+1)/(k+1-x_i))^w; p a Fraction."""
    num = (p.denominator-p.numerator)**n; den = p.denominator**n
    for v, w in counts.items():
        num *= (k+1)**w; den *= (k+1-v)**w
    return (num > den)-(num < den)

def replay_binom(r, data, budget=None):
    kind, certified, unique = outcome_of(r)
    mask = r['mask']
    if mask not in (0, 2): return [], 'size held: closed form'
    if kind != 'estimate' or not certified: return [], 'no certified estimate'
    x = [int(v) for v in data]
    if not x: return [], 'empty'
    n = len(x); S = sum(x); mx = max(x)
    counts = collections.Counter(x)
    if S == 0: return [], 'all-zero counts: closed form'
    if len(counts) == 1 and mask == 0: return [], 'constant counts: closed form'
    est = estimates(r)
    k = fint(est['size'])
    if k is None or k < mx: return [], 'returned size not an integer >= max x'
    out = []
    if mask == 0:
        out += binom_free_comparisons(r, counts, n, S, mx, k, est, unique)
    else:
        p = Fraction(r['held']['prob'])
        if not (0 < p < 1): return [], 'prob at an endpoint: closed form'
        out += binom_held_comparisons(counts, n, mx, k, p, unique)
    return out, None

def binom_free_comparisons(r, counts, n, S, mx, k, est, unique):
    out = []
    rigors = set()
    left = (+1, 'exact', 'n/a (k = max x)') if k == mx else binom_D_sign(counts, S, n, k-1)
    right = binom_D_sign(counts, S, n, k)
    rigors |= {left[1], right[1]}
    # enumeration of the range [max x, k + W] when short: the sign pattern of D is + ... + (<= 0) ...
    window = min(k+64, mx+3000)
    pattern = 'not enumerated (k - max x > 2500)'; pattern_ok = True
    if k-mx <= 2500:
        signs = []
        for j in range(mx, window+1):
            s_, rg, _ = binom_D_sign(counts, S, n, j); signs.append(s_); rigors.add(rg)
        first_nonpos = next((i for i, s_ in enumerate(signs) if s_ is None or s_ <= 0), len(signs))
        pattern_ok = (None not in signs and all(s_ > 0 for s_ in signs[:first_nonpos]) and
                      all(s_ <= 0 for s_ in signs[first_nonpos:]) and mx+first_nonpos == k)
        pattern = 'enumerated k in [%d, %d]: first D <= 0 at %d' % (mx, window, mx+first_nonpos)
        if None in signs: pattern += '; undecided signs'; pattern_ok = None
    if left[0] is None or right[0] is None or pattern_ok is None: verdict = 'open'
    elif left[0] > 0 and right[0] <= 0 and pattern_ok: verdict = 'holds'
    else: verdict = 'fails'
    out.append(comparison('search', 'size %d: D(%d) > 0 >= D(%d), D(k) = l(k+1) - l(k), unimodal on the enumerated range' % (k, k-1, k),
                          verdict, 'D(k-1): %s, D(k): %s; %s' % (left[2], right[2], pattern),
                          rigor='exact' if rigors == {'exact'} else 'interval',
                          note='range [%d, %d]' % (mx, window), search=dict(lo=mx, returned=k, window=window)))
    # tail: D < 0 at k + 2^j, sampled
    tail_ok = True; shown = []
    for j in range(0, 31):
        s_, rg, _ = binom_D_sign(counts, S, n, k+2**j)
        rigors.add(rg)
        if s_ is None: tail_ok = None; break
        if s_ >= 0: tail_ok = False; shown.append('k+2^%d: D >= 0' % j); break
    out.append(comparison('tail', 'D(k + 2^j) < 0 for j = 0..30 (DeRiggi: D changes sign once)',
                          'open' if tail_ok is None else 'holds' if tail_ok else 'fails',
                          'D < 0 at 31 sample points up to k + 2^30' if tail_ok else '; '.join(shown),
                          rigor='interval', note='sampled witness; the theorem is DeRiggi (1983)'))
    # coefficient: prob = S/(n k) within two ulps
    prob = est['prob']; target = Fraction(S, n*k)
    err = abs(Fraction(prob)-target)/target
    out.append(comparison('coef', 'prob = xbar/size within 2 ulp (profile maximizer at the size)',
                          'holds' if err <= 2*Fraction(EPS) else 'fails', 'relative error %s' % mp.nstr(mp.mpf(err.numerator)/err.denominator, 4),
                          rigor='exact', double=prob))
    # uniqueness: a tie needs an exact zero ratio; strict signs on both sides prove there is none
    if left[0] is None or right[0] is None: u = 'open'
    else:
        tie = left[0] == 0 or right[0] == 0
        u = 'holds' if tie == (not unique) else 'fails'
    out.append(comparison('tie', 'no other size attains the maximum (D(k-1) > 0 > D(k))', u,
                          'D(k-1) %s, D(k) %s; flagged unique = %s' % (left[2], right[2], unique),
                          rigor='exact' if rigors == {'exact'} else 'interval'))
    return out

def binom_held_comparisons(counts, n, mx, k, p, unique):
    out = []
    left = +1 if k == mx else held_R_sign(counts, n, p, k-1)
    right = held_R_sign(counts, n, p, k)
    signs = [held_R_sign(counts, n, p, j) for j in range(mx, k+41)] if k-mx <= 3000 else None
    mono = None if signs is None else all(a >= b for a, b in zip(signs, signs[1:]))
    first = None if signs is None else next((mx+i for i, s_ in enumerate(signs) if s_ <= 0), None)
    ok = left > 0 and right <= 0 and mono is not False and (first is None or first == k)
    out.append(comparison('search', 'size %d: R(%d) > 1 >= R(%d), R(k) = (1-p)^n prod (k+1)/(k+1-x_i)' % (k, k-1, k),
                          'holds' if ok else 'fails', 'R(k-1) %s 1, R(k) %s 1; %s' % ('>' if left > 0 else '<=', '<=' if right <= 0 else '>',
                          'sign pattern on [%d, %d]: first R <= 1 at %s, monotone %s' % (mx, k+40, first, mono) if signs is not None else 'not enumerated'),
                          rigor='exact'))
    far = [held_R_sign(counts, n, p, k+2**j) for j in range(0, 41, 4)]
    limit = (p.denominator-p.numerator)**n < p.denominator**n
    out.append(comparison('tail', 'R < 1 beyond k: R(k + 2^j) <= 1 sampled to 2^40, limit (1-p)^n < 1',
                          'holds' if all(s_ <= 0 for s_ in far) and limit else 'fails',
                          'signs %s, limit below one: %s' % (far, limit), rigor='exact', note='R decreases in k (README)'))
    tie = left == 0 or right == 0
    out.append(comparison('tie', 'no other size attains the maximum (R(k-1) > 1 > R(k))',
                          'holds' if tie == (not unique) else 'fails',
                          'R(k-1) %s 1, R(k) %s 1 (exact); flagged unique = %s' % (
                              '>' if left > 0 else '=' if left == 0 else '<', '<' if right < 0 else '=' if right == 0 else '>', unique),
                          rigor='exact', note='README: "possibly at two adjacent sizes"; the outcome flags uniqueness'))
    return out

# ---------------------------------------------------------------- generic machinery for the grid families

def ratio_sign(factors):
    """Sign of log prod (num/den)^w for positive integers: (+1 | 0 | -1). Exact."""
    num = den = 1
    for a, b, w in factors:
        num *= a**w; den *= b**w
    return (num > den)-(num < den)

class Model:
    """A count likelihood over integer coefficient vectors theta. Subclasses give the pmf of one value."""
    def __init__(self, counts):
        self.counts = counts; self.n = sum(counts.values()); self.S = sum(v*w for v, w in counts.items())
        self.mx = max(counts); self.mn = min(counts)
    def pmf_iv(self, theta, v): raise NotImplementedError
    def pmf_float(self, theta, v): raise NotImplementedError
    def pmf_frac(self, theta, v): raise NotImplementedError
    def logL_iv(self, theta):
        total = iv.mpf(0)
        for v, w in self.counts.items():
            term = self.pmf_iv(theta, v)
            if term is None: return None
            total += w*term
        return total
    def logL_float(self, theta):
        total = 0.0
        for v, w in self.counts.items():
            term = self.pmf_float(theta, v)
            if term is None: return -math.inf
            total += w*term
        return total
    def L_frac(self, theta):
        total = Fraction(1)
        for v, w in self.counts.items():
            q = self.pmf_frac(theta, v)
            if q == 0: return Fraction(0)
            total *= q**w
        return total

def lcf(a, b):
    if b < 0 or a < 0 or b > a: return None
    return math.lgamma(a+1)-math.lgamma(b+1)-math.lgamma(a-b+1)

class Hyper(Model):
    """theta = (N, m, k): log P(x) = log C(m, x) C(N-m, k-x) / C(N, k)."""
    def pmf_iv(self, t, v):
        N, m, k = t
        a, b, c = lc(m, v), lc(N-m, k-v), lc(N, k)
        return None if a is None or b is None or c is None else a+b-c
    def pmf_float(self, t, v):
        N, m, k = t
        a, b, c = lcf(m, v), lcf(N-m, k-v), lcf(N, k)
        return None if a is None or b is None or c is None else a+b-c
    def pmf_frac(self, t, v):
        N, m, k = t
        if not (0 <= v <= m and 0 <= k-v <= N-m and k <= N): return Fraction(0)
        return Fraction(math.comb(m, v)*math.comb(N-m, k-v), math.comb(N, k))

class Nhyper(Model):
    """theta = (N, m, r): total draws x until the r-th white ball: C(x-1, r-1) C(N-x, m-r) / C(N, m)."""
    def pmf_iv(self, t, v):
        N, m, r = t
        a, b, c = lc(v-1, r-1), lc(N-v, m-r), lc(N, m)
        return None if a is None or b is None or c is None else a+b-c
    def pmf_float(self, t, v):
        N, m, r = t
        a, b, c = lcf(v-1, r-1), lcf(N-v, m-r), lcf(N, m)
        return None if a is None or b is None or c is None else a+b-c
    def pmf_frac(self, t, v):
        N, m, r = t
        if not (1 <= r <= v and 0 <= m-r <= N-v and m <= N): return Fraction(0)
        return Fraction(math.comb(v-1, r-1)*math.comb(N-v, m-r), math.comb(N, m))

def verify_max(model, points, count, star, cap=300000):
    """Judge l(theta) <= l(star) on every point. Intervals for grids up to SCREEN_ABOVE points, binary64
    screening (margin 1e-6 (1 + |l|)) with interval judgement of the survivors above that.
    Returns dict(state, above, ties, evaluated, screened, best_gap, top) with state holds / fails / open."""
    with precision(DPS_STEPS[0]):
        Lstar = model.logL_iv(star)
    if Lstar is None: return dict(state='fails', reason='returned point has zero likelihood', above=[], ties=[], undecided=[], evaluated=0, screened=False, count=count, Lstar='-inf')
    Lstar_f = model.logL_float(star)
    above, ties, evaluated, undecided = [], [], 0, []
    screened = count > SCREEN_ABOVE
    if count > cap: return dict(state='open', reason='%d grid points exceed the cap %d' % (count, cap), count=count)
    margin = 1e-6*(1+abs(Lstar_f))
    lstar_at = {DPS_STEPS[0]: Lstar}
    def judge(theta):
        nonlocal evaluated
        evaluated += 1
        for d in DPS_STEPS[:4]:
            with precision(d):
                L = model.logL_iv(theta)
                if L is None: return
                if d not in lstar_at: lstar_at[d] = model.logL_iv(star)
                Ls = lstar_at[d]
                if mp.mpf(L.a) > mp.mpf(Ls.b): above.append((theta, show(L, 12))); return
                if mp.mpf(L.b) < mp.mpf(Ls.a): return
        try:
            a, b = model.L_frac(theta), model.L_frac(star)
        except NotImplementedError:
            undecided.append(theta); return
        if a == b: ties.append(theta)
        elif a > b: above.append((theta, 'exact'))
    for theta in points:
        if theta == star: continue
        if screened:
            f = model.logL_float(theta)
            if f < Lstar_f-margin: continue
        judge(theta)
    state = 'fails' if above else 'open' if undecided else 'holds'
    return dict(state=state, above=above[:3], ties=ties, undecided=undecided[:4], evaluated=evaluated,
                screened=screened, count=count, Lstar=show(Lstar, 14))

def zero_likelihood(text, star):
    return comparison('search', text, 'fails', 'the returned point %s has zero likelihood for the sample' % (star,), rigor='exact')

def one_dim(counts, lo, hi, star, factors, open_hi=False, window=4000, claim_tie=None):
    """Adjacent-ratio search of a unimodal profile on the integers lo..hi (hi None: open).
    factors(t) -> [(num, den, w)] of L(t)/L(t-1) for lo < t <= hi. star: the returned integer.
    Returns (verdict, hp text, tie state, enumerated bool)."""
    left = +1 if star == lo else ratio_sign(factors(star))
    right = -1 if (hi is not None and star == hi) else ratio_sign(factors(star+1))
    top = star+window if hi is None else min(hi, star+window)
    bottom = max(lo+1, star-window)
    signs = {t: ratio_sign(factors(t)) for t in range(bottom, top+1)}
    pattern = all((s > 0) for t, s in signs.items() if t <= star and t > lo) and \
              all((s <= 0) for t, s in signs.items() if t > star) and (star == lo or signs.get(star, 1) > 0)
    return dict(left=left, right=right, pattern=pattern, range=(bottom, top), signs=signs)

def tie_comparison(name_claim, ties_exist, unique, hp, rigor='exact', gap=None, near=False):
    """A certified estimate flagged unique must have no tie; a flagged tie without an exact tie is open (near tie)."""
    if ties_exist is None: return comparison('tie', name_claim, 'open', hp, rigor=rigor)
    if ties_exist and unique: return comparison('tie', name_claim, 'fails', hp, rigor=rigor,
                                                note='a tie exists (%s) but the outcome says unique = true' % rigor)
    if (not ties_exist) and (not unique):
        return comparison('tie', name_claim, 'open', hp, rigor=rigor,
                          note='flagged non-unique without an exact tie (core tolerance); exact log ratio %s' % gap)
    return comparison('tie', name_claim, 'holds', hp, rigor=rigor)

def log_ratio_text(factors):
    num = den = 1
    for a, b, w in factors:
        num *= a**w; den *= b**w
    return mp.nstr(mp.log(mp.mpf(num)/mp.mpf(den)), 6) if num != den else '0'

def one_dim_comparisons(counts, lo, hi, star, factors, unique, text, extra_tail=None):
    """search and tie for a one-coordinate adjacent-ratio search; extra_tail adds the tail comparison."""
    res = one_dim(counts, lo, hi, star, factors)
    ok = (res['left'] > 0 or star == lo) and (res['right'] <= 0 or (hi is not None and star == hi)) and res['pattern']
    out = [comparison('search', '%s = %d: L(t)/L(t-1) > 1 up to it, <= 1 from t+1 on [%d, %s]' % (text, star, lo, hi if hi is not None else 'inf'),
                      'holds' if ok else 'fails',
                      'ratio(t*) %s 1, ratio(t*+1) %s 1; pattern on [%d, %d] %s' % (
                          '>' if res['left'] > 0 else '=' if res['left'] == 0 else '<', '<=' if res['right'] <= 0 else '>',
                          res['range'][0], res['range'][1], 'ok' if res['pattern'] else 'violated'),
                      rigor='exact')]
    tie = (hi is None or star < hi) and res['right'] == 0
    gap = log_ratio_text(factors(star+1)) if (hi is None or star < hi) else None
    out.append(tie_comparison('no other integer attains the maximum: ratio(t*+1) != 1', tie, unique,
                              'ratio(t*+1) %s 1' % ('=' if res['right'] == 0 else '<' if res['right'] < 0 else '>'), gap=gap))
    return out, res

# ---------------------------------------------------------------- hyper

CLAIMS['hyper'] = {
    'm free (N, k held) / k free (N, m held)': ('last t in [max x, N - k + min x] with L(t)/L(t-1) > 1; unimodal (log-concave)',
                                                 'hyper/mle.hpp:11-16, 196-211 increment_m, 243-283 unimodal_peak, 608-614 swap'),
    'N free (m, k held)': ('last N >= m + k - min x with L(N)/L(N-1) > 1; D(u) < 0 for u > k (m+k)/mean(x)',
                           'hyper/mle.hpp:18-25, 213-228 increment_N, 653-681'),
    'N held (m, k free)': ('maximum over the grid m, k >= max x, m + k <= N + min x, lexicographically smallest within 64 ulp',
                           'hyper/mle.hpp:562-605, README Integer grids and population certificates'),
    'm or k held, N and the other free': ('every population N below the cutoff of B_s + h sum_{j<s} -log(1-j/N) < l*, profile over the free count',
                                          'hyper/mle.hpp:464-561, README Growing-population bound'),
    'all free': ('constant samples: likelihood one at (c, c, c)', 'hyper/mle.hpp:322-326'),
}

def hyper_m_factors(N, held, counts):
    """L(t)/L(t-1) for the free composition count t with the draw count (or the other composition) `held`."""
    def f(t): return [(t*(N-t+1-held+v), (t-v)*(N-t+1), w) for v, w in counts.items()]
    return f

def hyper_N_factors(m, k, counts):
    def f(t): return [((t-m)*(t-k), (t-m-k+v)*t, w) for v, w in counts.items()]
    return f

def binom_face_iv(model, s):
    """Binomial supremum with size s: sum_i log C(s, x_i) + S log S + (n s - S) log(n s - S) - n s log(n s)."""
    total = iv.mpf(0)
    for v, w in model.counts.items():
        a = lc(s, v)
        if a is None: return None
        total += w*a
    return total+binom_ent(model.S, model.n, s)

def corr_hyper_iv(model, s, N):
    """n sum_{j=1}^{s-1} -log(1 - j/N)."""
    total = iv.mpf(0)
    for j in range(1, s): total -= ilog(Fraction(N-j, N))
    return model.n*total

def find_cutoff(bound, Lstar, lo, limit=10**9):
    """Smallest N >= lo with bound(N).b < Lstar.a, for a bound decreasing in N; None if none <= limit."""
    ok = lambda N: mp.mpf(bound(N).b) < mp.mpf(Lstar.a)
    if ok(lo): return lo
    hi = max(lo, 1)
    while not ok(hi):
        hi *= 2
        if hi > limit: return None
    low = max(lo, hi//2)
    while hi-low > 1:
        mid = (low+hi)//2
        if ok(mid): hi = mid
        else: low = mid
    return hi

def grid_comparisons(model, points, count, star, unique, text, order_key=None):
    """search (verify_max) and tie for a finite grid."""
    res = verify_max(model, points, count, star)
    out = []
    if res.get('reason', '').startswith('returned point'): return [zero_likelihood(text, star)], res
    if res['state'] == 'open':
        out.append(comparison('search', text, 'open', res.get('reason'), rigor='interval')); return out, res
    smaller = [t for t in res['ties'] if order_key is not None and order_key(t) < order_key(star)]
    verdict = 'fails' if res['above'] or smaller else 'open' if res['undecided'] else 'holds'
    hp = '%d points %s; l* = %s; %s' % (res['count'], 'screened in binary64 then judged' if res['screened'] else 'enclosed',
                                      res['Lstar'], 'above: %s' % res['above'] if res['above'] else
                                      'exact ties: %s' % (res['ties'][:4] or 'none') + ('; undecided at 400 digits: %s' % res['undecided'] if res['undecided'] else ''))
    out.append(comparison('search', text, verdict, hp, rigor='interval', note='screened (binary64, margin 1e-6) above %d points' % SCREEN_ABOVE if res['screened'] else '',
                          search=dict(points=res['count'], evaluated=res['evaluated'], ties=len(res['ties']), screened=res['screened'])))
    out.append(tie_comparison('no other grid point attains the maximum', None if res['undecided'] else bool(res['ties']), unique,
                              'exact ties with the returned point: %s' % (res['ties'][:4] or 'none'), rigor='exact' if res['ties'] else 'interval'))
    return out, res

def replay_hyper(r, data, budget=None):
    kind, certified, unique = outcome_of(r)
    mask = r['mask']
    if mask == 7: return [], 'all held: evaluation only'
    if kind != 'estimate' or not certified: return [], 'no certified estimate'
    x = [int(v) for v in data]
    if not x: return [], 'empty'
    counts = collections.Counter(x); n = len(x); S = sum(x); mx = max(x); mn = min(x)
    est = estimates(r)
    try: N, m, k = (fint(est[c]) for c in ('N', 'm', 'k'))
    except KeyError: return [], 'no estimates'
    if None in (N, m, k): return [], 'non-integer or infinite coefficients'
    model = Hyper(counts); star = (N, m, k)
    out = []
    if mask in (6, 3):
        held, free = (k, m) if mask == 6 else (m, k)
        if held == 0 or free == 0: return [], 'a zero count: unidentified'
        lo, hi = mx, N-held+mn
        text = 'free %s' % ('m' if mask == 6 else 'k')
        out, _ = one_dim_comparisons(counts, lo, hi, free, hyper_m_factors(N, held, counts), unique, text)
    elif mask == 5:
        if m == 0 or k == 0 or S == 0: return [], 'zero count or all-zero data: unidentified or limit'
        lo = m+k-mn
        out, res = one_dim_comparisons(counts, lo, None, N, hyper_N_factors(m, k, counts), unique, 'free N')
        u0 = (k*(m+k)*n)//S+1       # smallest integer above k (m+k) / mean(x)
        f = hyper_N_factors(m, k, counts)
        signs = [ratio_sign(f(u0+2**j)) for j in range(0, 31, 3)] + [ratio_sign(f(u0))]
        out.append(comparison('tail', 'D(u) < 0 for every u > k (m+k)/mean(x); first integer %d >= N* %d' % (u0, N),
                              'holds' if all(s_ < 0 for s_ in signs) and N <= u0 else 'fails',
                              'ratio signs at u0 and u0 + 2^j (j = 0..30 step 3): %s' % signs, rigor='exact',
                              note='the bound is the dossier\'s (log1p(z) <= z); sampled signs above it'))
    elif mask == 2:
        lo = mx
        pts = ((m_, k_) for m_ in range(lo, N-lo+mn+1) for k_ in range(lo, N-m_+mn+1))
        count = sum(max(0, N-m_+mn-lo+1) for m_ in range(lo, N-lo+mn+1))
        out, _ = grid_comparisons(model, ((N, a, b) for a, b in pts), count, star, unique,
                                  'returned (m, k) = (%d, %d) maximizes the likelihood over the grid m, k >= %d, m + k <= N + %d' % (m, k, mx, mn),
                                  order_key=lambda t: (t[1], t[2]))
    elif mask in (1, 4):
        s = m if mask == 1 else k
        if s <= 1: return [], 'held count %d: Bernoulli or point mass (not replayed)' % s
        out = hyper_joint(model, counts, n, S, mx, mn, mask, s, star, unique)
    elif mask == 0:
        if len(counts) != 1:
            out = hyper_all_free(model, counts, n, mx, mn, star, unique)
            return out, None
        one = model.L_frac(star)
        witness = (N+1, N+1, k)
        out.append(comparison('search', 'constant sample: l(N, m, k) = 0 at (c, c, c) is the maximum (every pmf <= 1)',
                              'holds' if one == 1 else 'fails', 'exact likelihood at the returned point: %s' % one, rigor='exact'))
        tie = model.L_frac(witness) == 1
        out.append(tie_comparison('no other (N, m, k) attains likelihood one; witness (c+1, c+1, c)', tie, unique,
                                  'exact likelihood at %s: %s' % (witness, model.L_frac(witness))))
    else: return [], 'mask %d not replayed' % mask
    return out, None

def hyper_joint(model, counts, n, S, mx, mn, mask, s, star, unique):
    """One composition count s held, population N and the other count t free."""
    out = []
    face = binom_face_iv(model, s)
    N0s, m0, k0 = star
    with precision(DPS_STEPS[0]):
        Lstar = model.logL_iv(star)
    if Lstar is None: return [zero_likelihood('returned %s maximizes the likelihood' % (star,), star)]
    with precision(DPS_STEPS[0]):
        bound = lambda N: face+corr_hyper_iv(model, s, N)
        lo = s+mx-mn
        cutoff = None
        if face is not None and Lstar is not None and mp.mpf(face.b) < mp.mpf(Lstar.a):
            cutoff = find_cutoff(bound, Lstar, lo)
    if cutoff is None:
        if face is not None and Lstar is not None and mp.mpf(Lstar.b) < mp.mpf(face.a): v = 'fails'
        else: v = 'open'
        out.append(comparison('tail', 'binomial face B_%d + corr(N) < l* beyond a cutoff' % s, v,
                              'l* = %s, B = %s' % (show(Lstar, 12), show(face, 12)), rigor='interval',
                              note='no cutoff: l* does not exceed the binomial face'))
        out.append(comparison('search', 'every population below the cutoff', 'open', 'no cutoff'))
        return out
    with precision(DPS_STEPS[0]):
        b0 = bound(cutoff)
        bprev = bound(cutoff-1) if cutoff > lo else None
    out.append(comparison('tail', 'B_%d + h sum_{j<%d} -log(1 - j/N) < l* for N >= %d (first such N, bisected)' % (s, s, cutoff),
                          'holds', 'bound(%d) = %s < l* = %s; face B = %s' % (cutoff, show(b0, 12), show(Lstar, 12), show(face, 12)),
                          rigor='interval', note='the bound decreases in N (README); cutoff %d, returned N* = %d' % (cutoff, N0s)))
    def pts():
        for N in range(lo, cutoff):
            tlo, thi = mx, N-s+mn
            if thi < tlo: continue
            for t in peak_points(hyper_m_factors(N, s, counts), tlo, thi):
                yield (N, s, t) if mask == 1 else (N, t, s)
    pts_list = list(pts())
    res, _ = grid_comparisons(model, pts_list, len(pts_list), star, unique,
                              'returned (N, m, k) = %s maximizes the likelihood over every N in [%d, %d), the profile over the free count taken at its adjacent-ratio peak(s)' % (star, lo, cutoff))
    return out+res

def peak_points(factors, lo, hi):
    """The integers of [lo, hi] where a unimodal profile peaks: the last t with ratio(t) > 1 (lo if none), and t + 1
    when the adjacent ratio is exactly one (an exact tie). Exact bisection; unimodality is the dossier's theorem."""
    if hi == lo: return [lo]
    a, b = lo, hi+1
    while b-a > 1:
        mid = a+(b-a)//2
        if ratio_sign(factors(mid)) > 0: a = mid
        else: b = mid
    out = [a]
    if a < hi and ratio_sign(factors(a+1)) == 0: out.append(a+1)
    return out

def poisson_face_iv(model):
    """Maximised Poisson log likelihood: sum_i x_i log(mean) - mean - log x_i!."""
    mean = Fraction(model.S, model.n); total = iv.mpf(0)
    for v, w in model.counts.items():
        total += w*(v*ilog(mean)-I(mean)-ilog(math.factorial(v)))
    return total

def hyper_all_free(model, counts, n, mx, mn, star, unique, line_cap=600):
    """All three counts free, nonconstant sample: the Poisson envelope for both sizes >= Sr and N >= n0, a binomial
    face bound per smaller size, enumerating the points the bounds leave (README Growing-population bound)."""
    out = []
    T = model.S; X = mx
    with precision(DPS_STEPS[0]):
        Lstar = model.logL_iv(star)
        if Lstar is None: return [zero_likelihood('returned %s maximizes the likelihood' % (star,), star)]
        Q = poisson_face_iv(model)
        gap = Lstar-Q
    if mp.mpf(gap.a) <= 0:
        v = 'fails' if mp.mpf(gap.b) < 0 else 'open'
        return [comparison('tail', 'l* exceeds the Poisson face', v, 'l* - Q = %s' % show(gap, 10), rigor='interval'),
                comparison('search', 'finite fit is the global maximum', 'open', 'no Poisson cutoff')]
    g = mp.mpf(gap.a)
    with precision(DPS_STEPS[0]):
        Sr = max(X+1, int(mp.floor(X/(-mp.expm1(-g/(4*T)))))+2)
        n0 = max(X+1, int(mp.floor(X/(-mp.expm1(-g/(2*T)))))+2)
        env = lambda Sx, N0: Q+T*(-2*ilog(Fraction(Sx-X, Sx))-ilog(Fraction(N0-X, N0)))
        while not mp.mpf(env(Sr, n0).b) < mp.mpf(Lstar.a): Sr = int(Sr*1.05)+1; n0 = int(n0*1.05)+1
        envelope = env(Sr, n0)
    out.append(comparison('tail', 'Poisson envelope Q + T(-2 log(1 - X/S) - log(1 - X/N0)) < l* for sizes >= S = %d and N >= N0 = %d' % (Sr, n0),
                          'holds', 'envelope %s < l* = %s; Q = %s' % (show(envelope, 12), show(Lstar, 12), show(Q, 12)), rigor='interval',
                          note='dossier inequality for m, k >= S > X and N >= N0 > X (README); S, N0 from the replay\'s own formula'))
    if Sr-X > line_cap:
        return out+[comparison('search', 'binomial faces for sizes below S', 'open', '%d size lines exceed the cap %d' % (Sr-X, line_cap))]
    face_state = 'holds'; notes = []
    lines = []
    with precision(DPS_STEPS[0]):
        for s in range(X, Sr):
            face = binom_face_iv(model, s)
            if face is None: continue
            if not mp.mpf(face.b) < mp.mpf(Lstar.a):
                face_state = 'fails' if mp.mpf(Lstar.b) < mp.mpf(face.a) else 'open'
                notes.append('size %d: l* = %s vs face %s' % (s, show(Lstar, 10), show(face, 10)))
                continue
            bound = lambda N, face=face, s=s: face+corr_hyper_iv(model, s, N)
            lo = s+X-mn
            lines.append((s, lo, find_cutoff(bound, Lstar, lo)))
    if face_state != 'holds':
        return out+[comparison('search', 'every size below S', face_state, '; '.join(notes[:3]), rigor='interval',
                               note='a binomial face is not below l*: no cutoff for that size')]
    def pts():
        for s, lo, cut in lines:
            for N in range(lo, cut):
                tlo, thi = mx, N-s+mn
                if thi < tlo: continue
                for t in peak_points(hyper_m_factors(N, s, counts), tlo, thi): yield (N, s, t)
        for N in range(2*X-mn, n0):                      # both sizes >= S below N0 (usually empty)
            for s in range(Sr, (N+mn)//2+1):
                tlo, thi = mx, N-s+mn
                if thi < tlo: continue
                for t in peak_points(hyper_m_factors(N, s, counts), tlo, thi): yield (N, s, t)
    pl = list(pts())
    res, _ = grid_comparisons(model, pl, len(pl), star, unique,
                              'returned %s maximizes the likelihood over sizes s in [%d, %d) at every N below its cutoff, and the small-N part of s >= %d' % (star, X, Sr, Sr))
    return out+res

# ---------------------------------------------------------------- nhyper

CLAIMS['nhyper'] = {
    'm free (N, r held)': ('last t in [r, N - max x + r] with L(t)/L(t-1) = prod [t (N-t+1-x+r) / ((t-r)(N-t+1))]^w > 1',
                           'nhyper/mle.hpp:44-49 (header), 285-298'),
    'r free (N, m held)': ('last t in [max(1, max x - N + m), min(m, min x)] with L(t)/L(t-1) = prod [(x-t+1)(m-t+1) / ((t-1)(N-m-x+t))]^w > 1',
                           'nhyper/mle.hpp:235-267 (unimodal_peak)'),
    'N free, m held (r held or free)': ('lines r: N in [m + max x - r, max(., floor(m max x / r))]; beyond it every adjacent factor is <= 1 (N r >= x m)',
                                        'nhyper/mle.hpp:150-234, README Integer grids and population certificates'),
    'N held (m, r free)': ('finite grid r in [1, min x], m in [r, N - max x + r]', 'nhyper/mle.hpp:150-234'),
    'r held (N, m free) / all free': ('every N below the cutoff of B_r - sum_i c_N(x_i) < l* (B_r the shifted negative-binomial face, max over r when free); m by the adjacent ratio',
                                      'nhyper/mle.hpp:53-149, README Growing-population bound'),
}

def nhyper_m_factors(N, r, counts):
    def f(t): return [(t*(N-t+1-v+r), (t-r)*(N-t+1), w) for v, w in counts.items()]
    return f

def nhyper_r_factors(N, m, counts):
    def f(t): return [((v-t+1)*(m-t+1), (t-1)*(N-m-v+t), w) for v, w in counts.items()]
    return f

def nb_face_iv(model, r):
    """Shifted negative-binomial supremum for size r: sum_i log C(x_i - 1, r - 1) + n r log p + (S - n r) log(1 - p), p = n r / S."""
    n, S = model.n, model.S
    if n*r >= S: return None
    total = iv.mpf(0)
    for v, w in model.counts.items():
        a = lc(v-1, r-1)
        if a is None: return None
        total += w*a
    p = Fraction(n*r, S)
    return total+n*r*ilog(p)+(S-n*r)*ilog(1-p)

def corr_nhyper_iv(model, N):
    """sum_i sum_{h=1}^{x_i-1} -log(1 - h/N)."""
    total = iv.mpf(0)
    for v, w in model.counts.items():
        s_ = iv.mpf(0)
        for h in range(1, v): s_ -= ilog(Fraction(N-h, N))
        total += w*s_
    return total

def replay_nhyper(r, data, budget=None):
    kind, certified, unique = outcome_of(r)
    mask = r['mask']
    if mask == 7: return [], 'all held: evaluation only'
    if kind != 'estimate' or not certified: return [], 'no certified estimate'
    x = [int(v) for v in data]
    if not x: return [], 'empty'
    counts = collections.Counter(x); n = len(x); mx = max(x); mn = min(x)
    if mn == 0: return [], 'zero observations: unidentified or infeasible'
    est = estimates(r)
    try: N, m, rr = (fint(est[c]) for c in ('N', 'm', 'r'))
    except KeyError: return [], 'no estimates'
    if None in (N, m, rr): return [], 'non-integer or infinite coefficients'
    model = Nhyper(counts); star = (N, m, rr); S = model.S
    out = []
    if mask == 6:
        lo, hi = rr, N-mx+rr
        out, _ = one_dim_comparisons(counts, lo, hi, m, nhyper_m_factors(N, rr, counts), unique, 'free m')
    elif mask == 3:
        lo, hi = max(1, mx-(N-m)), min(m, mn)
        out, _ = one_dim_comparisons(counts, lo, hi, rr, nhyper_r_factors(N, m, counts), unique, 'free r')
    elif mask == 2:
        pts = [(N, m_, r_) for r_ in range(1, min(mn, N)+1) for m_ in range(r_, N-mx+r_+1)]
        out, _ = grid_comparisons(model, pts, len(pts), star, unique,
                                  'returned (m, r) = (%d, %d) maximizes the likelihood over the grid r in [1, %d], m in [r, N - %d + r]' % (m, rr, mn, mx),
                                  order_key=lambda t: (t[1], t[0], t[2]))
    elif mask in (1, 5):
        out = nhyper_lines(model, counts, mx, mn, m, rr if mask == 5 else None, star, unique)
    elif mask in (4, 0):
        if len(counts) == 1 and mask == 0:
            c = next(iter(counts)); one = model.L_frac(star); witness = (N+1, m+1, rr)
            out.append(comparison('search', 'constant sample: likelihood one at the returned point is the maximum', 'holds' if one == 1 else 'fails',
                                  'exact likelihood %s' % one, rigor='exact'))
            out.append(tie_comparison('no other (N, m, r) attains likelihood one; witness (N+1, m+1, r)', model.L_frac(witness) == 1, unique,
                                      'exact likelihood at %s: %s' % (witness, model.L_frac(witness))))
        else:
            out = nhyper_face_search(model, counts, n, mx, mn, rr if mask == 4 else None, star, unique)
    else: return [], 'mask %d not replayed' % mask
    return out, None

def nhyper_lines(model, counts, mx, mn, m, r_held, star, unique):
    """m held, N free; r held (mask 5) or free (mask 1): lines r of N in [m + max x - r, max(lo, floor(m max x / r))]."""
    out = []
    rs = [r_held] if r_held is not None else list(range(1, min(mn, m)+1))
    pts = []; tails = []
    for r in rs:
        lo = m+mx-r; hi = max(lo, (m*mx)//r)
        pts += [(N, m, r) for N in range(lo, hi+1)]
        N1 = hi+1
        ok = all(v*m <= N1*r and N1-v-m+r > 0 for v in counts)
        tails.append((r, hi, ok))
    out.append(comparison('tail', 'beyond N = max(m + max x - r, floor(m max x / r)) every adjacent factor is <= 1 (x m <= N r), for each r on the lines',
                          'holds' if all(t[2] for t in tails) else 'fails',
                          'lines (r, last N): %s' % [(t[0], t[1]) for t in tails][:6], rigor='exact',
                          note='the dossier: factors below one beyond m max(x)/r; checked exactly at the first integer above'))
    res, _ = grid_comparisons(model, pts, len(pts), star, unique,
                              'returned %s maximizes the likelihood over the %d line(s) r = %s' % (star, len(rs), rs[:6]),
                              order_key=lambda t: (t[1], t[0], t[2]))
    return out+res

def nhyper_face_search(model, counts, n, mx, mn, r_held, star, unique, cap_n=20000):
    out = []
    S = model.S
    rs = [r_held] if r_held is not None else list(range(1, mn+1))
    with precision(DPS_STEPS[0]):
        Lstar = model.logL_iv(star)
        if Lstar is None: return [zero_likelihood('returned %s maximizes the likelihood' % (star,), star)]
        faces = {r: nb_face_iv(model, r) for r in rs}
        faces = {r: f for r, f in faces.items() if f is not None}
        if not faces: return [comparison('tail', 'negative-binomial face', 'open', 'no face (constant sample)')]
        fmax = max(faces.values(), key=lambda f: mp.mpf(f.b))
        bound = lambda N: fmax+corr_nhyper_iv(model, N)
        cutoff = None
        if mp.mpf(fmax.b) < mp.mpf(Lstar.a): cutoff = find_cutoff(bound, Lstar, mx)
    if cutoff is None:
        v = 'fails' if mp.mpf(Lstar.b) < mp.mpf(fmax.a) else 'open'
        return [comparison('tail', 'B_r - sum c_N(x_i) < l* beyond a cutoff', v, 'l* %s, largest face %s' % (show(Lstar, 12), show(fmax, 12)), rigor='interval'),
                comparison('search', 'every population below the cutoff', 'open', 'no cutoff')]
    out.append(comparison('tail', 'max_r B_r + sum_i sum_{h<x_i} -log(1 - h/N) < l* for N >= %d (first such N, bisected)' % cutoff,
                          'holds', 'bound %s < l* = %s; largest face %s' % (show(bound(cutoff), 12), show(Lstar, 12), show(fmax, 12)),
                          rigor='interval', note='shifted negative-binomial face and correction decreasing in N (README); returned N* = %d' % star[0]))
    if cutoff > cap_n: return out+[comparison('search', 'every population below the cutoff', 'open', 'cutoff %d exceeds %d' % (cutoff, cap_n))]
    def pts():
        for N in range(mx, cutoff):
            for r in rs:
                lo, hi = r, N-mx+r
                if hi < lo: continue
                for t in peak_points(nhyper_m_factors(N, r, counts), lo, hi): yield (N, t, r)
    pl = list(pts())
    res, _ = grid_comparisons(model, pl, len(pl), star, unique,
                              'returned %s maximizes the likelihood over every N in [%d, %d), r = %s, m at its adjacent-ratio peak(s)' % (star, mx, cutoff, rs[:4]),
                              order_key=lambda t: (t[1], t[0], t[2]))
    return out+res

# ---------------------------------------------------------------- bbinom

CLAIMS['bbinom'] = {
    'size free, alpha and beta held (mask 6)': ('enumerate N in [max x, ceil(max x (alpha+beta-1)/alpha)]; every pmf decreases once alpha (N+1) >= x (alpha+beta-1)',
                                                'bbinom/mle.hpp:514-528, README Details (line 155)'),
    'size free, beta held (mask 4)': ('enumerate N until m (N+1-M) >= h M (beta-1) H_N, H_N = sum_{j<=N} 1/(beta+j); profile over alpha at each size',
                                      'bbinom/mle.hpp:465-512, README Held-beta tail certificate'),
    'size free, alpha held (mask 2)': ('enumerate N until the signed bounds B(N, c) and F + correction(N, c) fall below the incumbent; profile over beta at each size',
                                       'bbinom/mle.hpp:402-463, README Held alpha: negative-binomial face and uniform size bound'),
}

class BB:
    """log pmf of the beta-binomial at integer size N and rational shapes, via prefix sums of logs."""
    def __init__(self, counts, a, b):
        self.counts = counts; self.n = sum(counts.values()); self.S = sum(v*w for v, w in counts.items())
        self.mx = max(counts); self.a = a; self.b = b

def bb_prefix_iv(q, upto):
    out = [iv.mpf(0)]
    for j in range(upto): out.append(out[-1]+ilog(q+j))
    return out

def bb_logL_iv(counts, N, a, b, pa=None, pb=None, pab=None):
    """Interval log likelihood at exact rationals a, b (Fractions)."""
    pa = pa or bb_prefix_iv(a, max(counts)); pb = pb or bb_prefix_iv(b, N); pab = pab or bb_prefix_iv(a+b, N)
    total = iv.mpf(0)
    for v, w in counts.items():
        if v > N: return None
        total += w*(lc(N, v)+pa[v]+pb[N-v]-pab[N])
    return total

def bb_pmf_frac(N, a, b, v):
    num = Fraction(math.comb(N, v)); den = Fraction(1)
    for j in range(v): num *= a+j
    for j in range(N-v): num *= b+j
    for j in range(N): den *= a+b+j
    return num/den

class BBHeld(Model):
    """Single-integer model theta = (N,) with both shapes held."""
    def __init__(self, counts, a, b):
        super().__init__(counts); self.a = a; self.b = b
        self._pa = {}; self._pb = {}; self._pab = {}
    def _prefix(self, cache, q, N):
        key = (N, iv.dps)
        if key not in cache: cache[key] = bb_prefix_iv(q, N)
        return cache[key]
    def logL_iv(self, theta):
        N = theta[0]
        return bb_logL_iv(self.counts, N, self.a, self.b, self._prefix(self._pa, self.a, self.mx), self._prefix(self._pb, self.b, N), self._prefix(self._pab, self.a+self.b, N))
    def logL_float(self, theta):
        N = theta[0]; a, b = float(self.a), float(self.b)
        total = 0.0
        for v, w in self.counts.items():
            total += w*(lcf(N, v)+sum(math.log(a+j) for j in range(v))+sum(math.log(b+j) for j in range(N-v))-sum(math.log(a+b+j) for j in range(N)))
        return total
    def L_frac(self, theta):
        N = theta[0]; total = Fraction(1)
        for v, w in self.counts.items(): total *= bb_pmf_frac(N, self.a, self.b, v)**w
        return total

def replay_bbinom(r, data, budget=None):
    kind, certified, unique = outcome_of(r)
    mask = r['mask']
    if mask not in (2, 4, 6): return [], 'size held or all held: not an integer search'
    if kind != 'estimate' or not certified: return [], 'no certified estimate'
    x = [int(v) for v in data]
    if not x: return [], 'empty'
    counts = collections.Counter(x); mx = max(x); mn = min(x)
    est = estimates(r)
    N = fint(est['size'])
    if N is None: return [], 'size not an integer'
    if mx == 0: return [], 'all-zero counts: size zero, closed form (faces module)'
    if mn == mx: return [], 'constant counts: point mass (faces module)'
    a, b = Fraction(est['alpha']), Fraction(est['beta'])
    if N < mx: return [zero_likelihood('size %d maximizes the likelihood' % N, (N,))], None
    if mask == 6: return bbinom_both_held(counts, mx, N, a, b, unique), None
    if mask == 4: return bbinom_beta_held(counts, mx, N, a, b, unique, est), None
    return bbinom_alpha_held(counts, mx, N, a, b, unique, est), None

def bbinom_both_held(counts, mx, N, a, b, unique):
    out = []
    # first size from which every pmf decreases: alpha (N+1) >= mx (alpha + beta - 1)
    need = Fraction(mx)*(a+b-1)/a
    Ndec = max(mx, math.ceil(need)-1)
    model = BBHeld(counts, a, b)
    out.append(comparison('tail', 'every pmf decreases from size %d on: alpha (N+1) >= x (alpha + beta - 1) for x <= max x' % Ndec,
                          'holds' if a*(Ndec+1) >= mx*(a+b-1) else 'fails',
                          'alpha (N+1) = %s >= max x (alpha+beta-1) = %s' % (mp.nstr(mp.mpf((a*(Ndec+1)).numerator)/(a*(Ndec+1)).denominator, 10),
                                                                                mp.nstr(mp.mpf((mx*(a+b-1)).numerator)/(mx*(a+b-1)).denominator, 10)),
                          rigor='exact', note='ratio P_{N+1}/P_N = (N+1)(N-x+beta)/((N+1-x)(N+alpha+beta)) <= 1 iff alpha(N+1) >= x(alpha+beta-1)'))
    pts = [(M,) for M in range(mx, Ndec+2)]
    res, _ = grid_comparisons(model, pts, len(pts), (N,), unique,
                              'size %d maximizes the likelihood over the sizes %d..%d; later sizes decrease' % (N, mx, Ndec+1), order_key=lambda t: t[0])
    return out+res

def bb_float_profile(counts, N, a=None, b=None):
    """Binary64 profile log likelihood function of the free shape at size N, via prefix sums."""
    S0 = {}
    def ll(free):
        aa, bb = (a, free) if a is not None else (free, b)
        total = 0.0
        pa = [0.0]; pb = [0.0]; pab = [0.0]
        for j in range(max(counts)): pa.append(pa[-1]+math.log(aa+j))
        for j in range(N): pb.append(pb[-1]+math.log(bb+j)); pab.append(pab[-1]+math.log(aa+bb+j))
        for v, w in counts.items(): total += w*(lcf(N, v)+pa[v]+pb[N-v]-pab[N])
        return total
    return ll

def bb_profile_float(counts, N, a=None, b=None, grid=80):
    """Binary64 supremum over the free shape at size N: log grid then golden-section polish of every local maximum."""
    ll = bb_float_profile(counts, N, a, b)
    xs = [10**(-6+12*i/(grid-1)) for i in range(grid)]
    vals = [ll(t) for t in xs]
    best = -math.inf; where = []
    for i in range(1, grid-1):
        if vals[i] >= vals[i-1] and vals[i] >= vals[i+1]:
            lo, hi = math.log(xs[i-1]), math.log(xs[i+1])
            g = (math.sqrt(5)-1)/2
            c1, c2 = hi-g*(hi-lo), lo+g*(hi-lo)
            f1, f2 = ll(math.exp(c1)), ll(math.exp(c2))
            for _ in range(60):
                if f1 > f2: hi, c2, f2 = c2, c1, f1; c1 = hi-g*(hi-lo); f1 = ll(math.exp(c1))
                else: lo, c1, f1 = c1, c2, f2; c2 = lo+g*(hi-lo); f2 = ll(math.exp(c2))
            v = max(f1, f2, vals[i]); where.append(math.exp((lo+hi)/2)); best = max(best, v)
    return best, where

def bb_profile_numeric(counts, N, a=None, b=None, grid=80):
    """Numerical (50-digit) supremum of the log likelihood over the free shape at size N, and its argmax:
    every local maximum of a log grid is refined by a root of the score; returns (value, argmax) or (None, None)."""
    _, where = bb_profile_float(counts, N, a, b, grid)
    if not where: return None, None
    def score(t):
        aa, bb = (a, t) if a is not None else (t, b)
        total = mp.mpf(0)
        for v, w in counts.items():
            if a is not None: first = sum(1/(bb+j) for j in range(N-v))
            else: first = sum(1/(aa+j) for j in range(v))
            second = sum(1/(aa+bb+j) for j in range(N))
            total += w*(first-second)
        return total
    best = (None, None)
    for t0 in where:
        try: root = mp.findroot(score, (mp.mpf(t0)*0.98, mp.mpf(t0)*1.02), solver='anderson', tol=mp.mpf(10)**(-40), maxsteps=200)
        except Exception: continue
        aa, bb = (mp.mpf(a.numerator)/a.denominator, root) if a is not None else (root, mp.mpf(b.numerator)/b.denominator)
        value = bb_ll_mp(counts, N, aa, bb)
        if best[0] is None or value > best[0]: best = (value, root)
    return best

def bb_ll_mp(counts, N, a, b):
    pa = [mp.mpf(0)]; pb = [mp.mpf(0)]; pab = [mp.mpf(0)]
    for j in range(max(counts)): pa.append(pa[-1]+mp.log(a+j))
    for j in range(N): pb.append(pb[-1]+mp.log(b+j)); pab.append(pab[-1]+mp.log(a+b+j))
    return sum(w*(mp.log(math.comb(N, v))+pa[v]+pb[N-v]-pab[N]) for v, w in counts.items())

def bb_search_numeric(counts, sizes, star_N, a, b, unique, text, hold):
    """Profile over the free shape at every size, compared with the returned size's. Binary64 values (golden-section
    polished) screen the sizes; sizes within 1e-6 of the returned one are refined to 50 digits."""
    ha, hb = (a if hold == 'alpha' else None), (b if hold == 'beta' else None)
    with precision(DPS_STEPS[0]):
        ref_f = bb_profile_float(counts, star_N, ha, hb)[0]
        star_val = bb_profile_numeric(counts, star_N, ha, hb)[0]
        if star_val is None or not math.isfinite(ref_f):
            return comparison('search', text, 'open', 'no profile maximum found at the returned size', rigor='numerical'), []
        vals = {star_N: star_val}; screened = 0
        for N in sizes:
            if N == star_N: continue
            f = bb_profile_float(counts, N, ha, hb)[0]
            if f < ref_f-1e-6: vals[N] = mp.mpf(f); screened += 1; continue
            vals[N] = bb_profile_numeric(counts, N, ha, hb)[0]
        ref = star_val
        worse = [(N, v) for N, v in vals.items() if v is not None and N != star_N and v > ref+mp.mpf(10)**-25]
        tied = [N for N, v in vals.items() if v is not None and N != star_N and abs(v-ref) <= mp.mpf(10)**-25]
        missing = [N for N, v in vals.items() if v is None]
        near = max(((v, N) for N, v in vals.items() if v is not None and N != star_N), default=None)
    verdict = 'fails' if worse else 'open' if (tied or missing) else 'holds'
    hp = 'profile at %d = %s; best other size %s' % (star_N, mp.nstr(ref, 20), ('%d: %s (gap %s)' % (near[1], mp.nstr(near[0], 20), mp.nstr(ref-near[0], 4))) if near else 'none')
    if missing: hp += '; no maximum found at sizes %s' % missing[:5]
    return comparison('search', text, verdict, hp, rigor='numerical',
                      note='profile suprema by a log grid and golden-section polish in binary64; %d of %d sizes more than 1e-6 below the returned size are screened that way, the rest refined to 50 digits by a score root (not an enclosure)' % (screened, len(sizes)),
                      search=dict(sizes=[min(sizes), max(sizes)], returned=star_N, ties=tied)), tied

def bbinom_beta_held(counts, mx, N, a, b, unique, est):
    out = []
    h = sum(counts.values()); pos = sum(w for v, w in counts.items() if v > 0)
    # first size with m (N+1-M) >= h M (beta-1) H_N, H_N = sum_{j<=N} 1/(beta+j); beta <= 1: every size from max x
    if b <= 1: Nc = mx
    else:
        Nc = mx; H = sum(Fraction(1)/(b+j) for j in range(mx+1))
        while pos*(Nc+1-mx) < h*mx*(b-1)*H:
            Nc += 1; H += Fraction(1)/(b+Nc)
            if Nc > 20000: break
    ok = pos*(Nc+1-mx) >= h*mx*(b-1)*(sum(Fraction(1)/(b+j) for j in range(Nc+1)) if b > 1 else 0)
    out.append(comparison('tail', 'm (N+1-M) >= h M (beta-1) H_N from N = %d on, so profile(N+1) <= profile(N) beyond' % Nc,
                          'holds' if ok and Nc <= 20000 else 'open',
                          'm = %d, h = %d, M = %d, beta = %s' % (pos, h, mx, mp.nstr(mp.mpf(b.numerator)/b.denominator, 10)), rigor='exact',
                          note='dossier: left side over H increases with N, so the comparison holds for all later sizes'))
    sizes = list(range(mx, Nc+2))
    if len(sizes) > 400: return out+[comparison('search', 'sizes', 'open', '%d sizes exceed the cap' % len(sizes))]
    res = bb_search_numeric(counts, sizes, N, None, b, unique, 'size %d maximizes the alpha-profile over the sizes %d..%d' % (N, mx, Nc+1), 'beta')
    if isinstance(res, tuple): c, tied = res
    else: c, tied = res, []
    out.append(c)
    out.append(tie_comparison('no other size attains the maximum of the profile', bool(tied) if tied else (None if c['verdict'] == 'open' else False), unique,
                              'sizes with profile within 1e-25 of the returned size: %s' % tied, rigor='numerical'))
    return out

def bbinom_alpha_held(counts, mx, N, a, b, unique, est):
    out = []
    h = sum(counts.values()); S = sum(v*w for v, w in counts.items())
    coeff = iv.mpf(0)
    with precision(DPS_STEPS[0]):
        for v, w in counts.items():
            for j in range(v): coeff += w*(ilog(a+j)-ilog(j+1))
        mu = Fraction(S, h)
        F = coeff+S*ilog(mu/(a+mu))-h*I(a)*ilog(1+mu/a)
        quad = sum(w*Fraction(v*(v+1), 2) for v, w in counts.items())
        Lstar = bb_logL_iv(counts, N, a, b)
    if not mp.mpf(F.b) < mp.mpf(Lstar.a):
        v = 'fails' if mp.mpf(Lstar.b) < mp.mpf(F.a) else 'open'
        return [comparison('tail', 'face F < l*', v, 'l* %s, F %s' % (show(Lstar, 12), show(F, 12)), rigor='interval'),
                comparison('search', 'sizes below the cutoff', 'open', 'no cutoff')]
    cs = [Fraction(2)**e for e in range(-24, 41, 2)]
    def closes_at(M, c):
        with precision(DPS_STEPS[0]):
            common = coeff+S*ilog(Fraction(M, M-mx))
            small = common+h*I(a)*ilog((a+c*M)/(a+(1+c)*M))
            if not mp.mpf(small.b) < mp.mpf(Lstar.a): return False
            big = F+I(quad)/I(M-mx)+h*I(a*a)/2*(1/I(c*M)+1/I(c*M)**2)
            return bool(mp.mpf(big.b) < mp.mpf(Lstar.a))
    cut = None
    for c in cs:
        hi_ = N+1
        while not closes_at(hi_, c):
            hi_ = mx+2*(hi_-mx)
            if hi_ > 200000: break
        else:
            lo_ = max(N+1, mx+1)
            if closes_at(lo_, c): first = lo_
            else:
                a_, b_ = lo_, hi_
                while b_-a_ > 1:
                    mid = (a_+b_)//2
                    if closes_at(mid, c): b_ = mid
                    else: a_ = mid
                first = b_
            if cut is None or first < cut[0]: cut = (first, c)
    if cut is None: return [comparison('tail', 'B(N, c) and F + correction(N, c) < l* for some size', 'open', 'no size up to 200000 closes with the c grid', rigor='interval')]
    # tighten to the first closing size by bisection back to N+1 (the bounds decrease in N for fixed c)
    M0, c = cut
    out.append(comparison('tail', 'B(N, c) = common + h a log((a + cN)/(a + (1+c)N)) and F + sum x(x+1)/2/(N-M) + h a^2/2 (1/(cN) + 1/(cN)^2) < l* for N >= %d, c = %s' % (M0, c),
                          'holds', 'l* = %s, face F = %s' % (show(Lstar, 12), show(F, 12)), rigor='interval',
                          note='both bounds decrease in N at fixed c (dossier); %d is the smallest size that closes for some c of the grid 2^-24..2^40' % M0))
    sizes = list(range(mx, M0))
    if len(sizes) > 400: return out+[comparison('search', 'sizes', 'open', '%d sizes exceed the cap' % len(sizes))]
    res = bb_search_numeric(counts, sizes, N, a, None, unique, 'size %d maximizes the beta-profile over the sizes %d..%d' % (N, mx, M0-1), 'alpha')
    c2, tied = res if isinstance(res, tuple) else (res, [])
    out.append(c2)
    out.append(tie_comparison('no other size attains the maximum of the profile', bool(tied) if tied else (None if c2['verdict'] == 'open' else False), unique,
                              'sizes with profile within 1e-25 of the returned size: %s' % tied, rigor='numerical'))
    return out

# ---------------------------------------------------------------- cmb

CLAIMS['cmb'] = {
    'size free, prob and nu held (mask 6)': ('enumerate N from max x until the decreasing bound (nu > 0: normalizer term at max x + 1; nu < 0: endpoint terms) falls below the incumbent',
                                             'cmb/mle.hpp:1406-1467 (cmb_size_mle), README Details line 200'),
    'size free with prob and/or nu free (masks 0, 2, 4)': ('profile over the free coefficient(s) at each size (concave exponential family); tail by K-truncated entropy bounds and blocks',
                                                           'cmb/mle.hpp:920-1401, README Free-size profile bounds'),
}

def logsumexp_float(ts):
    m = max(ts); return m+math.log(sum(math.exp(t-m) for t in ts))

class CMBHeld(Model):
    """theta = (N,), prob and nu held (rationals); l(N) = sum_i log P_N(x_i), Z_N the finite normalizer."""
    def __init__(self, counts, p, nu):
        super().__init__(counts); self.p = p; self.nu = nu; self._cache = {}
    def _parts(self, N):
        logp, logq = ilog(self.p), ilog(1-self.p)
        nu = I(self.nu)
        terms = [nu*lc(N, k)+k*logp+(N-k)*logq for k in range(N+1)]
        Z = iv.mpf(0)
        for t in terms: Z += iv.exp(t)
        return nu, logp, logq, ilog_iv(Z)
    def logL_iv(self, theta):
        N = theta[0]; key = (N, iv.dps)
        if key in self._cache: return self._cache[key]
        if N < self.mx: return None
        nu, logp, logq, logZ = self._parts(N)
        total = iv.mpf(0)
        for v, w in self.counts.items(): total += w*(nu*lc(N, v)+v*logp+(N-v)*logq-logZ)
        self._cache[key] = total
        return total
    def logL_float(self, theta):
        N = theta[0]; p, nu = float(self.p), float(self.nu)
        ts = [nu*lcf(N, k)+k*math.log(p)+(N-k)*math.log1p(-p) for k in range(N+1)]
        z = logsumexp_float(ts)
        return sum(w*(nu*lcf(N, v)+v*math.log(p)+(N-v)*math.log1p(-p)-z) for v, w in self.counts.items())
    def L_frac(self, theta): raise NotImplementedError

def ilog_iv(x): return iv.log(x)

def replay_cmb(r, data, budget=None):
    kind, certified, unique = outcome_of(r)
    mask = r['mask']
    if mask in (1, 3, 5, 7): return [], 'size held: not an integer search'
    if kind != 'estimate' or not certified: return [], 'no certified estimate'
    x = [int(v) for v in data]
    if not x: return [], 'empty'
    counts = collections.Counter(x); mx = max(x); mn = min(x); n = len(x)
    est = estimates(r)
    N = fint(est['size'])
    if N is None: return [], 'size not an integer'
    if mx == 0: return [], 'all-zero counts: size one or zero, closed form'
    if N < mx: return [zero_likelihood('size %d maximizes the likelihood' % N, (N,))], None
    if mask == 6:
        p, nu = Fraction(est['prob']), Fraction(est['nu'])
        if not (0 < p < 1): return [], 'prob at an endpoint: exact reduction'
        if nu == 0: return [], 'nu = 0: smallest feasible size (not replayed)'
        return cmb_both_held(counts, n, mx, N, p, nu, unique), None
    return cmb_profile(counts, n, mx, N, est, mask, unique), None

def cmb_both_held(counts, n, mx, N, p, nu, unique):
    out = []; S = sum(v*w for v, w in counts.items())
    model = CMBHeld(counts, p, nu)
    with precision(DPS_STEPS[0]):
        eta = ilog(p/(1-p)); Lstar = model.logL_iv((N,))
        if Lstar is None: return [zero_likelihood('size %d maximizes the likelihood' % N, (N,))]
        def bound(M):
            if nu > 0:
                total = iv.mpf(0)
                for v, w in counts.items():
                    inner = iv.mpf(0)
                    for j in range(v, mx+1): inner += ilog(Fraction(j+1, M+1-j))
                    total += w*(I(nu)*inner+(v-(mx+1))*eta)
                return total
            total = iv.mpf(0)
            for v, w in counts.items(): total += w*I(nu)*lc(M+1, v)
            e = (M+1)*eta
            low = iv.mpf([max(0, mp.mpf(e.a)), max(0, mp.mpf(e.b))])
            return total+eta*S-n*low
        cutoff = find_cutoff(bound, Lstar, mx)
    if cutoff is None:
        return [comparison('tail', 'decreasing bound below l* from some size on', 'open', 'no size up to 1e9 closes', rigor='interval')]
    with precision(DPS_STEPS[0]): b0 = bound(cutoff)
    out.append(comparison('tail', 'bound(N) < l* for N >= %d (nu %s 0: %s), covering every size M >= N + 1' % (
        cutoff, '>' if nu > 0 else '<', 'sum_i [nu log (C(M, x_i)/C(M, max x + 1)) + (x_i - max x - 1) eta]' if nu > 0 else 'nu sum_i log C(M, x_i) + eta S - n max(0, M eta)'),
        'holds', 'bound(%d) = %s < l* = %s' % (cutoff, show(b0, 12), show(Lstar, 12)), rigor='interval',
        note='bound decreasing in the size (README); the core stops at the first size below the incumbent with margin 1e-10 n'))
    pts = [(M,) for M in range(mx, cutoff+1)]
    res, _ = grid_comparisons(model, pts, len(pts), (N,), unique,
                              'size %d maximizes the likelihood over the sizes %d..%d; sizes beyond are excluded by the bound' % (N, mx, cutoff),
                              order_key=lambda t: t[0])
    return out+res

def cmb_ll_mp(counts, N, eta, nu):
    """log likelihood at size N, log odds eta, dispersion nu, 50-digit; also returns the weights' score parts."""
    hs = [mp.log(math.comb(N, k)) for k in range(N+1)]
    terms = [nu*hs[k]+eta*k for k in range(N+1)]
    m = max(terms); Z = sum(mp.exp(t-m) for t in terms); logZ = m+mp.log(Z)
    S = sum(v*w for v, w in counts.items()); n = sum(counts.values())
    return nu*sum(w*hs[v] for v, w in counts.items())+eta*S-n*logZ, hs, terms, m, Z

def cmb_profile_numeric(counts, N, free, start=None, nu0=None, eta0=None, iters=80):
    """Maximise over the free coefficients (a subset of {'eta', 'nu'}) by damped Newton in 40 digits; the log
    likelihood is concave in (eta, nu). Returns (value, eta, nu) or None."""
    S = sum(v*w for v, w in counts.items()); n = sum(counts.values())
    if N == 1:      # size one: both outcomes have C(1, k) = 1, so nu drops out of the likelihood (Bernoulli)
        free = tuple(f for f in free if f != 'nu'); nu0 = 1
        if not free:
            return cmb_ll_mp(counts, 1, mp.mpf(eta0), mp.mpf(1))[0], mp.mpf(eta0), mp.mpf(1)
    hsum = sum(w*mp.log(math.comb(N, v)) for v, w in counts.items())
    hs = [mp.log(math.comb(N, k)) for k in range(N+1)]
    eta = mp.mpf(eta0 if eta0 is not None else math.log(max(S/(n*N), 1e-9)/max(1-S/(n*N), 1e-9)))
    nu = mp.mpf(nu0 if nu0 is not None else 1)
    def evaluate(e, v):
        terms = [v*hs[k]+e*k for k in range(N+1)]
        m = max(terms); ws = [mp.exp(t-m) for t in terms]; Z = sum(ws)
        P = [w_/Z for w_ in ws]
        Ek = sum(k*P[k] for k in range(N+1)); Eh = sum(hs[k]*P[k] for k in range(N+1))
        Ekk = sum(k*k*P[k] for k in range(N+1)); Ehh = sum(hs[k]**2*P[k] for k in range(N+1)); Ekh = sum(k*hs[k]*P[k] for k in range(N+1))
        value = v*hsum+e*S-n*(m+mp.log(Z))
        return value, Ek, Eh, Ekk-Ek**2, Ehh-Eh**2, Ekh-Ek*Eh
    value, Ek, Eh, vk, vh, ckh = evaluate(eta, nu)
    for _ in range(iters):
        g_e, g_n = S-n*Ek, hsum-n*Eh
        if 'eta' in free and 'nu' in free:
            det = vk*vh-ckh**2
            if det <= 0: return None
            de, dn = (g_e*vh-g_n*ckh)/(n*det), (g_n*vk-g_e*ckh)/(n*det)
        elif 'eta' in free: de, dn = g_e/(n*vk), mp.mpf(0)
        else: de, dn = mp.mpf(0), g_n/(n*vh)
        if max(abs(de), abs(dn)) < mp.mpf(10)**-18: break
        step = mp.mpf(1)
        for _ in range(40):
            new = evaluate(eta+step*de, nu+step*dn)
            if new[0] >= value-mp.mpf(10)**-30: break
            step /= 2
        else: return None
        eta += step*de; nu += step*dn; value, Ek, Eh, vk, vh, ckh = new
        if abs(eta) > 500 or abs(nu) > 500: return None
    else: return None
    return value, eta, nu

def cmb_profile(counts, n, mx, N, est, mask, unique):
    """Free prob and/or nu (masks 0, 2, 4): numerical profile at each size from max x to max(N + 8, max x + 40)."""
    free = {0: ('eta', 'nu'), 2: ('nu',), 4: ('eta',)}[mask]
    nu_held = est['nu'] if mask == 4 else None
    p_held = est['prob'] if mask == 2 else None
    eta_held = math.log(p_held/(1-p_held)) if p_held is not None else None
    top = max(N+8, mx+40)
    sizes = list(range(mx, top+1))
    vals = {}
    with precision(DPS_STEPS[0]):
        for M in sizes:
            r_ = cmb_profile_numeric(counts, M, free, nu0=nu_held if mask == 4 else None, eta0=eta_held)
            vals[M] = None if r_ is None else r_[0]
        ref = vals.get(N)
        if ref is None:
            res = comparison('search', 'size %d maximizes the profile over the sizes %d..%d' % (N, mx, top), 'open', 'no profile maximum at the returned size',
                             rigor='numerical')
            tied = None
        else:
            worse = [(M, v) for M, v in vals.items() if v is not None and M != N and v > ref+mp.mpf(10)**-12]
            tied = [M for M, v in vals.items() if v is not None and M != N and abs(v-ref) <= mp.mpf(10)**-12]
            missing = [M for M, v in vals.items() if v is None]
            near = max(((v, M) for M, v in vals.items() if v is not None and M != N), default=None)
            hp = 'profile at %d = %s; best other size %s' % (N, mp.nstr(ref, 20), ('%d: %s (gap %s)' % (near[1], mp.nstr(near[0], 20), mp.nstr(ref-near[0], 4))) if near else 'none')
            if missing: hp += '; no finite maximum at sizes %s' % missing[:6]
            res = comparison('search', 'size %d maximizes the profile over the sizes %d..%d' % (N, mx, top),
                             'fails' if worse else 'open' if (tied or missing) else 'holds', hp, rigor='numerical',
                             note='profile maxima by damped Newton on the concave exponential-family log likelihood (40 digits); sizes with no finite maximum are escapes to a limit, listed')
    out = [res]
    out.append(comparison('tail', 'K-truncated entropy bound and block bounds exclude the sizes beyond %d' % top, 'info',
                          'not replayed', rigor='none',
                          note='the dossier bounds (README Free-size profile bounds) need the entropy-dual optimization; the enumerated window is a witness, not a proof'))
    flat = N == 1 and 'nu' in free      # at size one nu is not identified: exact non-uniqueness
    out.append(tie_comparison('no other size attains the maximum of the profile (and, at size one, nu is identified)',
                              True if flat else (None if tied is None else bool(tied)), unique,
                              'size one: nu does not enter the likelihood' if flat else 'sizes within 1e-12 of the returned size: %s' % tied,
                              rigor='exact' if flat else 'numerical'))
    return out

# ---------------------------------------------------------------- triang

CLAIMS['triang'] = {
    'free mode (masks 0-3)': ('the likelihood is convex in c between observations, so a free mode is one of the distinct observations (within the held endpoints); '
                              'each mode is profiled over the free endpoints (concave in the reciprocal distances) and the best is returned, ties to the smallest mode',
                              'triang/mle.hpp:193-201 (modes), 207-280 (candidate, pruning), 244 (ties), README Details'),
}

class TriBoth(Model):
    """theta = (c,), both endpoints held (Fractions): L(c) = (2/(b-a))^n prod_{x<c} (x-a)/(c-a) prod_{x>c} (b-x)/(b-c)."""
    def __init__(self, counts, a, b):
        super().__init__(counts); self.a = a; self.b = b
    def _factors(self, c):
        a, b = self.a, self.b
        if c < a or c > b: return None
        out = []
        for x, w in self.counts.items():
            if x < c:
                if x <= a: return None
                out.append(((x-a)/(c-a), w))
            elif x > c:
                if x >= b: return None
                out.append(((b-x)/(b-c), w))
        return out
    def logL_iv(self, theta):
        f = self._factors(theta[0])
        if f is None: return None
        total = self.n*(ilog(Fraction(2)/(self.b-self.a)))
        for q, w in f: total += w*ilog(q)
        return total
    def logL_float(self, theta):
        f = self._factors(theta[0])
        if f is None: return -math.inf
        return self.n*math.log(2/float(self.b-self.a))+sum(w*math.log(float(q)) for q, w in f)
    def L_frac(self, theta):
        f = self._factors(theta[0])
        if f is None: return Fraction(0)
        total = (Fraction(2)/(self.b-self.a))**self.n
        for q, w in f: total *= q**w
        return total

def tri_ll_mp(xs, c, s, t):
    """n log 2 - n log(s + t) + sum_{x<c} log(1 - (c-x)/s) + sum_{x>c} log(1 - (x-c)/t); s, t may be None (that end at c)."""
    n = len(xs); total = n*mp.log(2)
    width = (s or 0)+(t or 0)
    if width <= 0: return -mp.inf
    total -= n*mp.log(width)
    for x in xs:
        if x < c:
            if s is None or s <= c-x: return -mp.inf
            total += mp.log(1-(c-x)/s)
        elif x > c:
            if t is None or t <= x-c: return -mp.inf
            total += mp.log(1-(x-c)/t)
    return total

def tri_profile(xs, c, a_held, b_held):
    """Profile of the log likelihood over the free endpoints at mode c. Binary64 L-BFGS in log(s - dmax), log(t - emax),
    then a 40-digit root polish of the scores. Returns (value_float, value_mp or None, s, t)."""
    import numpy as np
    from scipy.optimize import minimize
    left = [c-x for x in xs if x < c]; right = [x-c for x in xs if x > c]
    n = len(xs)
    # s: distance c - a. a held: fixed; no left points and a free: s = 0 (a = c).
    s_fixed = None if a_held is None else c-a_held
    t_fixed = None if b_held is None else b_held-c
    sfree = s_fixed is None and bool(left); tfree = t_fixed is None and bool(right)
    s0 = 0.0 if (s_fixed is None and not left) else s_fixed
    t0 = 0.0 if (t_fixed is None and not right) else t_fixed
    if s0 is not None and left and s0 <= max(left): return -math.inf, None, None, None
    if t0 is not None and right and t0 <= max(right): return -math.inf, None, None, None
    L = np.array(left); R = np.array(right)
    dmax = max(left) if left else 0.0; emax = max(right) if right else 0.0
    def value(sv, tv):
        v = n*math.log(2)-n*math.log(sv+tv) if sv+tv > 0 else -math.inf
        if left: v += float(np.log1p(-L/sv).sum())
        if right: v += float(np.log1p(-R/tv).sum())
        return v
    def unpack(z):
        i = 0; sv, tv = s0, t0
        if sfree: sv = dmax+math.exp(z[i]); i += 1
        if tfree: tv = emax+math.exp(z[i])
        return sv, tv
    if not (sfree or tfree):
        return value(s0, t0), None, s0, t0
    z0 = np.array([math.log(max(dmax, 1e-300))]*(1 if sfree else 0)+[math.log(max(emax, 1e-300))]*(1 if tfree else 0))
    best = None
    for shift in (0.0, 2.0, -2.0):
        z = z0+shift
        res = minimize(lambda zz: -value(*unpack(zz)), z, method='BFGS', options=dict(gtol=1e-10, maxiter=200))
        if best is None or res.fun < best.fun: best = res
    sv, tv = unpack(best.x)
    return -best.fun, None, sv, tv

def tri_polish(xs, c, a_held, b_held, s, t):
    """40-digit stationary point of the endpoint scores near (s, t); returns the log likelihood or None."""
    xs_ = list(xs); cc = c
    left = [cc-x for x in xs_ if x < cc]; right = [x-cc for x in xs_ if x > cc]; n = len(xs_)
    s_fixed = None if a_held is None else cc-a_held; t_fixed = None if b_held is None else b_held-cc
    sfree = s_fixed is None and bool(left); tfree = t_fixed is None and bool(right)
    sv0 = s_fixed if s_fixed is not None else (mp.mpf(0) if not left else mp.mpf(s))
    tv0 = t_fixed if t_fixed is not None else (mp.mpf(0) if not right else mp.mpf(t))
    def scores(*v):
        i = 0; sv, tv = sv0, tv0
        if sfree: sv = v[i]; i += 1
        if tfree: tv = v[i]
        out = []
        if sfree: out.append(-n/(sv+tv)+sum(d/(sv*(sv-d)) for d in left))
        if tfree: out.append(-n/(sv+tv)+sum(e/(tv*(tv-e)) for e in right))
        return out
    start = ([mp.mpf(s)] if sfree else [])+([mp.mpf(t)] if tfree else [])
    if not start: return tri_ll_mp(xs_, cc, sv0 if sv0 else None, tv0 if tv0 else None)
    try:
        root = mp.findroot(scores, start if len(start) > 1 else start[0], tol=mp.mpf(10)**-30, maxsteps=100)
    except Exception: return None
    vals = [root] if not isinstance(root, mp.matrix) else [root[i] for i in range(len(root))]
    i = 0; sv, tv = sv0, tv0
    if sfree: sv = vals[i]; i += 1
    if tfree: tv = vals[i]
    return tri_ll_mp(xs_, cc, sv if (sv != 0 or left) else None, tv if (tv != 0 or right) else None)

def replay_triang(r, data, budget=None):
    kind, certified, unique = outcome_of(r)
    mask = r['mask']
    if mask & 4: return [], 'mode held: one mode, no enumeration'
    if kind != 'estimate' or not certified: return [], 'no certified estimate'
    xs = sorted(float(v) for v in data)
    if not xs or xs[0] == xs[-1]: return [], 'constant or empty sample'
    est = estimates(r); c_star = est['c']
    fa, fb = bool(mask & 1), bool(mask & 2)
    a_held = float(r['held']['a']) if fa else None; b_held = float(r['held']['b']) if fb else None
    forced_a = fa and a_held == xs[0]; forced_b = fb and b_held == xs[-1]
    if forced_a: modes = [a_held]
    elif forced_b: modes = [b_held]
    else: modes = sorted({z for z in xs if (not fa or z >= a_held) and (not fb or z <= b_held)})
    if len(modes) <= 1: return [], 'a single mode'
    if c_star not in modes: return [], 'returned mode %r is not an observation' % c_star
    text = 'returned mode %r is the best of the %d candidate modes' % (c_star, len(modes))
    if fa and fb:
        counts = collections.Counter(Fraction(z) for z in xs)
        model = TriBoth(counts, Fraction(a_held), Fraction(b_held))
        pts = [(Fraction(m_),) for m_ in modes]
        out, _ = grid_comparisons(model, pts, len(pts), (Fraction(c_star),), unique, text, order_key=lambda t: t[0])
        for c in out:
            if c['name'] == 'search': c['rigor'] = 'interval'
        return out, None
    # standardize exactly (x - min)/(max - min): the log likelihood of every mode shifts by the same - n log(scale)
    x0 = Fraction(xs[0]); scale = Fraction(xs[-1])-x0
    tr = lambda v: (Fraction(v)-x0)/scale
    Q = lambda q: mp.mpf(q.numerator)/q.denominator
    xs_f = [float(tr(v)) for v in xs]
    a_f = None if a_held is None else float(tr(a_held)); b_f = None if b_held is None else float(tr(b_held))
    with precision(DPS_STEPS[0]):
        xs_mp = [Q(tr(v)) for v in xs]
        a_mp = None if a_held is None else Q(tr(a_held)); b_mp = None if b_held is None else Q(tr(b_held))
        floats = {}
        for m_ in modes:
            v, _, s, t = tri_profile(xs_f, float(tr(m_)), a_f, b_f)
            floats[m_] = (v, s, t)
        ref_f = floats[c_star][0]
        polished = {}
        near = [m_ for m_, (v, _, _) in floats.items() if v >= ref_f-1e-5 or m_ == c_star]
        for m_ in near:
            _, s, t = floats[m_]
            polished[m_] = tri_polish(xs_mp, Q(tr(m_)), a_mp, b_mp, s, t)
        ref = polished.get(c_star)
        if ref is None:
            return [comparison('search', text, 'open', 'no 40-digit stationary endpoint pair at the returned mode (float profile %r)' % (floats[c_star],), rigor='numerical')], None
        worse = [(m_, v) for m_, v in polished.items() if m_ != c_star and v is not None and v > ref+mp.mpf(10)**-20]
        tied = [m_ for m_, v in polished.items() if m_ != c_star and v is not None and abs(v-ref) <= mp.mpf(10)**-20]
        missing = [m_ for m_, v in polished.items() if v is None]
        others = [(v, m_) for m_, v in polished.items() if m_ != c_star and v is not None]
        runner = max(others) if others else None
    hp = 'standardized profile at %r = %s; runner-up among refined modes %s; %d of %d modes screened in binary64 (more than 1e-5 below)' % (
        c_star, mp.nstr(ref, 20), ('%r: %s (gap %s)' % (runner[1], mp.nstr(runner[0], 20), mp.nstr(ref-runner[0], 4))) if runner else 'none',
        len(modes)-len(near), len(modes))
    verdict = 'fails' if worse else 'open' if (missing) else 'holds'
    out = [comparison('search', text, verdict, hp, rigor='numerical',
                      note='data standardized exactly; endpoint profile by L-BFGS (binary64) then a 40-digit root of the endpoint scores; modes more than 1e-5 below the returned one are not refined',
                      search=dict(modes=len(modes), refined=len(near)))]
    ties = bool(tied)
    out.append(tie_comparison('no other mode attains the maximum profile (numerical, within 1e-20)', ties, unique,
                              'modes within 1e-20 of the returned profile: %s' % tied, rigor='numerical'))
    return out, None

REPLAYS = {'binom': replay_binom, 'hyper': replay_hyper, 'nhyper': replay_nhyper, 'bbinom': replay_bbinom, 'cmb': replay_cmb,
           'triang': replay_triang}
