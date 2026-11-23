"""Certificate replay, mechanism D: exact face inequalities (step 8, job replay-faces, TASK-214.28).

Registered into cert-replay.py's REPLAYS; that script owns the driver, the
verdict rules and the strict-layer ratchet (dev/design/certificate-replay.md).
Each replay reads the claim from the snapshot record (outcome kind, reason,
certified, unique, limit model and payload, returned coefficients), states
the inequality that the dossier mathematics attaches to that claim, and
evaluates it exactly: in rational arithmetic on the binary64 data
(fractions.Fraction; every double is a dyadic rational) or, where the
predicate contains exp, in mpmath interval arithmetic (outward rounded).
Nothing is read from the core beyond the record, so a changed outcome
mapping (step 5b) is judged by the same predicates.

Where the core's own floating-point evaluation of a predicate is simple, the
record also gives a mirror of it: Python floats for binary64, numpy.longdouble
for the x87 long double of native builds (CPPUNIV_EXTENDED_REAL). Mirrors are
context, not evidence; they show where binary64 and exact arithmetic disagree.
Code read at master 9141de55 (after step 5b; the post-5b snapshots of master are the
baselines). A change to one of these decisions must update this module in the
same commit.

Verdicts: holds, the exact predicate selects what the outcome claims; fails,
it selects something else and the outcome claims it (certified estimate or
limit, or a nonexistence); open, an uncertified outcome contradicted by the
predicate (flagged by the core, listed but not judged) or an interval
enclosure that does not decide.

n is the sample size, S = sum x, Q = sum x^2, k a multiplicity; source in brackets.

binom      all free  moment      Poisson limit iff 0 < mean <= population variance, n S <= n Q - S^2
                                 [README existence; mle.hpp `x_bar / var_pop <= 1.0`, unpadded]
                     support     constant positive counts: estimate size = x, prob = 1 [README Details]
                     unique      all-zero counts: not unique [README existence and Details]
gompertz   all free  moment      exponential limit iff n Q >= 2 S^2 (profile right derivative at b = 0)
                                 [README Details; mle.hpp `sum_xsq/n >= 2*mean^2`, unpadded]
           a held    moment      exponential limit iff S <= a Q / 2 [mle.hpp held-a guard, unpadded]
                     support     constant sample: unbounded; unique: all zero with a held is flat in b
pig        all free  moment      Poisson limit iff n F <= S^2, F = sum x(x-1) (variance <= mean)
           mean held moment      Poisson limit iff n mu^2 + F <= 2 mu S (binary counts: n mu <= 2 #ones)
                                 [README existence; mle.hpp free_shape, binary_shape]
                     support     all zero: shape 0 attained (mean held), zero limit (shape held)
                     unique      all zero, both free: mean unidentified [README Zero member]
foldnorm   all free  quartic     a returned mu = 0 needs H(0) = 1 - mean(x^4)/(3 mean(x^2)^2) <= 0
                                 (H(0) > 0: the boundary is a local minimum; H(0) = 0 only for the
                                 {0, c} exception) [README face section; mle.hpp face_score, margin 32 eps]
           sigma held location   mu = 0 iff mean(x^2) <= sigma^2 [README existence; mle.hpp `m2 > s*s`]
                     support     constant sample (or all equal to a held mu): unbounded
bbinom     size held size2       N = 2: finite iff 4 n0 n2 > n1^2, else the binomial face; a finite
                                 claim also returns the closed form alpha = (n1+2n2) n1/D,
                                 beta = (2n0+n1) n1/D, D = 4 n0 n2 - n1^2 [mle.hpp N == 2 branch]
                     support     point mass (all 0 or all N), two-point {0, N}; else no degenerate claim
                     unique      size 0 held, or size free with all-zero counts: likelihood one for every
                                 positive free shape, certified estimate flagged non-unique
                                 [README, last paragraph; mle.hpp Unidentified branch]
cauchy     all free  multiplicity largest multiplicity k: 2k < n estimate, 2k = n unattained (finite
                                 collapse supremum), 2k > n unbounded; balanced two-point sample:
                                 estimate, not unique [README existence]
           loc held  multiplicity k = #(x == location), same three cases
slash      all free  multiplicity 2k > n unbounded, 2k = n unattained or estimate (decided by a 1e-8 n
                                 margin, not replayed), 2k < n estimate [README existence]
           mu held   multiplicity k = #(x == mu): 2k > n unbounded, 2k = n unattained, else estimate
hcauchy    mu free/held multiplicity k = #(x == min) or #(x == mu): 2k < n estimate, 2k = n
                                 unattained, 2k > n unbounded [README existence]
sgomp      all free  support     constant or all-zero sample: unbounded, else a finite MLE [README]
           b held    face-score  eta = 0 iff sum(1 - 2 exp(-b x)) <= 0 [README reason; mle.hpp face_score]
           eta held  support     all zero: unbounded, else estimate
dnorm, dweibull, dgamma, dlaplace, cmp, cmb, lgser, zeta, yulesimon
                     support     point-mass and two-point predicates on the integer support, and
                                 the existence statements of each README (range >= 2, some count > 1,
                                 cell closures containing a held mean, median interval)
                     unique      dweibull shape1 held and all zero; dlaplace median interval;
                                 cmp all zero (nu unidentified)
pois+zero=inflated   zero-fraction  pi > 0 iff #zeros/n > exp(-mean) (interval) [zip_mle.hpp]
                     support     pi held below one and all zero: rate limit at zero
                     unique      all zero: (lambda, pi = 1) not identified [zip_mle.hpp comment]
norm +truncate=tnorm window      exponential face iff population variance >= variance of the
                                 mean-matched t2 = 0 member: (mean - a)^2, (b - mean)^2, or the
                                 truncated exponential on (a, b) (interval); held mean: uniform face
                                 iff mean((x-mu)^2) <= ((a-mu)^2 + (a-mu)(b-mu) + (b-mu)^2)/3
                                 [tnorm_mle.hpp header comment]; bounds not held are min and max.
                     support     constant sample with an estimated bound (each held bound equal to it):
                                 unbounded even with mean and/or sd held [tnorm_mle.hpp
                                 estimated_bounds_collapse; truncation.hpp held_normal_window_mle]
zeta +truncate=zipf  support     all at 1 or all at N: point mass; N = 1: estimate, not unique
"""
import collections, math
from fractions import Fraction
import mpmath as mp
import numpy as np
from strict_common import number

iv = mp.iv
EPS = 2.0**-52
LD = np.longdouble
TWO52 = 4503599627370496.0

# ---------------------------------------------------------------- records

def comparison(name, claim, verdict, hp, double=None, margin=None, rigor='exact', note='', info=None):
    return dict(name=name, claim=claim, verdict=verdict, hp=hp, double=double, margin=margin,
                rigor=rigor, note=note, info=info)

def approx(q, digits=17):
    q = Fraction(q)
    return mp.nstr(mp.mpf(q.numerator)/q.denominator, digits) if q else '0'

def exact(q):
    text = str(Fraction(q))
    return text if len(text) <= 48 else '%s (a %d-character rational)' % (approx(q), len(text))

def gap(a, b):
    """Relative difference |a - b| / max(|a|, |b|) as text."""
    a, b = Fraction(a), Fraction(b); d = max(abs(a), abs(b))
    return approx(abs(a-b)/d, 3) if d else '0'

def outcome(r):
    o = r['result'].get('outcome') or {}
    return o.get('kind'), o.get('reason'), bool(o.get('certified')), o.get('unique', True)

def estimates(r):
    return {k: number(v) for k, v in (r['result'].get('estimates') or {}).items()}

def fmt(v):
    v = float(v)
    return str(int(v)) if v.is_integer() else repr(v)

def label(r):
    """The claim as a label: estimate, nonexistence:<reason>, point-mass@p, two-point@lo,hi, limit:<model>."""
    kind, reason, _, _ = outcome(r); res = r['result']
    if kind == 'estimate': return 'estimate'
    if kind == 'nonexistence': return 'nonexistence:%s' % reason
    if kind != 'limit': return None
    model = res.get('model') or ''
    le = {k: number(v) for k, v in (res.get('limit_estimates') or {}).items()}
    if model == 'unavailable:point-mass' and 'point' in le: return 'point-mass@%s' % fmt(le['point'])
    if model == 'unavailable:two-point' and 'lower' in le:
        return 'two-point@%s,%s' % (fmt(le['lower']), fmt(le['upper']))
    if model == 'binom' and le.get('size') == 1: return 'two-point@0,1'
    return 'limit:%s' % model

def claims(r):
    """Whether the outcome asserts its claim: a certified estimate or limit, or a nonexistence."""
    kind, _, certified, _ = outcome(r)
    return certified or kind == 'nonexistence'

def decide(ok, r):
    return 'holds' if ok else 'fails' if claims(r) else 'open'

def support(r, expected, why, source, name='support', note=''):
    """The claim label must lie in `expected` (a set of labels, or (description, predicate))."""
    got = label(r)
    if isinstance(expected, tuple): text, ok = expected[0], expected[1](got)
    else: text, ok = ' or '.join(sorted(expected)), got in expected
    return comparison(name, 'outcome %s' % got, decide(ok, r), '%s: %s' % (why, text),
                      note='source: %s%s' % (source, '; '+note if note else ''))

def not_degenerate(got):
    return got is not None and not got.startswith(('point-mass', 'two-point'))
NOT_DEGENERATE = ('no point-mass or two-point claim', not_degenerate)

def unique(r, expected, why, source):
    kind, _, certified, uniq = outcome(r)
    ok = kind == 'estimate' and bool(uniq) == expected
    return comparison('unique', '%s with unique = %s' % (kind, str(bool(uniq)).lower()),
                      'holds' if ok else 'fails' if certified else 'open',
                      '%s: unique = %s' % (why, str(expected).lower()), note='source: %s' % source)

def judged(r):
    return outcome(r)[0] in ('estimate', 'limit', 'nonexistence')

def ints(data):
    out = [int(v) for v in data]
    if any(a != b for a, b in zip(out, data)): raise ValueError('non-integer data')
    return out

def ivq(q):
    """Exact rational as an mpmath interval."""
    q = Fraction(q)
    return iv.mpf(q.numerator)/q.denominator

def interval_side(value, threshold):
    """+1 if the interval lies strictly above threshold, -1 strictly below, 0 undecided."""
    lo, hi = mp.mpf(value.a), mp.mpf(value.b)
    t = mp.mpf(threshold)
    return 1 if lo > t else -1 if hi < t else 0

# ---------------------------------------------------------------- binom

def replay_binom(r, data, budget=None):
    kind, _, _, _ = outcome(r)
    if r['mask'] != 0: return [], 'size or prob held: closed form or integer search (no face inequality)'
    if kind not in ('estimate', 'limit'): return [], None
    x = ints(data); n = len(x); S = sum(x); Q = sum(v*v for v in x)
    if S == 0:
        return [unique(r, False, 'all-zero counts: (n = 0, p = 0) and every (n, p = 0) attain the maximum',
                       'binom README, existence ("do not identify the parameters") and Details')], None
    if len(set(x)) == 1:
        e = estimates(r)
        ok = kind == 'estimate' and e.get('size') == x[0] and e.get('prob') == 1
        return [comparison('support', 'outcome %s, size %s, prob %s' % (kind, e.get('size'), e.get('prob')),
                           decide(ok, r), 'constant positive counts: estimate size = %d, prob = 1' % x[0],
                           note='source: binom README Details; mle.hpp uniq.size() == 1')], None
    difference = Fraction(n*S+S*S-n*Q, n*n)          # mean - population variance
    truth = 'finite' if difference > 0 else 'poisson'
    claim = 'poisson' if kind == 'limit' else 'finite'
    sx = sq = 0.0
    for v in x: sx += float(v); sq += float(v)*float(v)
    xbar = sx/n; var = 0.0
    for v in x: d = float(v)-xbar; var += d*d
    var /= n
    ratio = xbar/var if var else math.inf
    core = 'poisson' if ratio <= 1.0 else 'finite'
    model = r['result'].get('model')
    note = ('binary64 mirror (mle.hpp: double sums, two-pass variance, x_bar/var_pop <= 1) decides %s%s; '
            'relative gap |mean - var|/max = %s' % (core, ' (DISAGREES with exact)' if core != truth else '',
                                                    gap(Fraction(S, n), Fraction(n*Q-S*S, n*n))))
    if kind == 'limit' and model != 'pois': note += '; limit model %r' % model
    return [comparison('moment', '%s (mean - population variance %s 0)' % (claim, '<=' if claim == 'poisson' else '>'),
                       decide(truth == claim, r), 'mean - var = %s' % exact(difference),
                       double='x_bar/var_pop - 1 = %.17g' % (ratio-1), margin='none (unpadded)', note=note)], None

# ---------------------------------------------------------------- gompertz

def gompertz_mirror(x, extended):
    """Mirror of gompertz_mle_impl's free face guard: (decision, lhs, rhs, path)."""
    n = len(x)
    with np.errstate(over='ignore', invalid='ignore'):
        if extended:
            total = LD(0)
            for v in x: total += LD(v)
            unit = float(total/LD(n))
        else:
            total = 0.0
            for v in x: total += v
            unit = total/n
    standard = unit > 0 and math.isfinite(unit)
    z = [v/unit for v in x] if standard else list(x)
    sx = sq = 0.0
    for v in z: sx += v; sq += v*v
    m = sx/n
    lhs, rhs = sq/n, 2.0*m*m
    path = 'data / mean %r' % unit if standard else 'raw data (mean %r not finite)' % unit
    return ('limit' if lhs >= rhs else 'finite'), lhs, rhs, path

def replay_gompertz(r, data, budget=None):
    kind, _, _, _ = outcome(r); mask = r['mask']
    if mask in (2, 3): return [], 'b held: analytic' if mask == 2 else 'evaluation only'
    if not judged(r): return [], None
    x = [float(v) for v in data]; fx = [Fraction(v) for v in x]; n = len(x)
    S = sum(fx); Q = sum(v*v for v in fx)
    if mask == 0:
        if len(set(x)) == 1:
            return [support(r, {'nonexistence:unbounded'}, 'constant sample', 'gompertz README Details')], None
        if kind == 'nonexistence': return [support(r, {'estimate', 'limit:exp'}, 'nonconstant sample', 'gompertz README Details')], None
        value = n*Q-2*S*S                                   # >= 0: exponential limit
        truth = 'limit' if value >= 0 else 'finite'
        claim = 'limit' if kind == 'limit' else 'finite'
        mirrors = []
        for name, extended in (('long double', True), ('double', False)):
            decision, lhs, rhs, path = gompertz_mirror(x, extended)
            mirrors.append('%s intermediates (%s): mean(z^2) = %r, 2 mean(z)^2 = %r, decides %s%s' % (
                name, path, lhs, rhs, decision, ' (DISAGREES with exact)' if decision != truth else ''))
        return [comparison('moment', '%s (n Q - 2 S^2 %s 0)' % (claim, '>=' if claim == 'limit' else '<'),
                           decide(truth == claim, r), 'n Q - 2 S^2 = %s; relative gap %s' % (approx(value, 6), gap(n*Q, 2*S*S)),
                           double='; '.join(mirrors), margin='none (unpadded)',
                           note='native builds use the long-double mirror, double-only builds the double one')], None
    a = Fraction(r['held']['a'])
    if max(x) == 0:
        return [unique(r, False, 'all-zero data with a held: the likelihood is flat in b', 'gompertz README Details')], None
    value = 2*S-a*Q                                         # <= 0: exponential limit
    truth = 'limit' if value <= 0 else 'finite'
    claim = 'limit' if kind == 'limit' else 'finite'
    big = max(x); sy = sy2 = 0.0
    for v in x: y = v/big; sy += y; sy2 += y*y
    decision = 'finite' if sy > (float(a)*big)*(sy2/2) else 'limit'
    return [comparison('moment', '%s (S - a Q/2 %s 0)' % (claim, '<=' if claim == 'limit' else '>'),
                       decide(truth == claim, r), 'S - a Q/2 = %s; relative gap %s' % (approx(value/2, 6), gap(S, a*Q/2)),
                       double='sum(y) = %r, a max(x) sum(y^2)/2 = %r (y = x/max x), decides %s%s' % (
                           sy, float(a)*big*(sy2/2), decision, ' (DISAGREES with exact)' if decision != truth else ''),
                       margin='none (unpadded)')], None

# ---------------------------------------------------------------- pig

def pig_mirror(n, S, F, mean, average, mean_free, ones=None):
    """Mirror of pig's Poisson-face guard (mle.hpp free_shape and binary_shape): (decision, path)."""
    if ones is not None:
        product = mean*n; target = 2.0*ones
        residual = math.fma(mean, float(n), -product) if math.isfinite(product) else 0.0
        poisson = product < target or (product == target and residual <= 0)
        return ('poisson' if poisson else 'finite'), 'binary counts: FMA comparison of mean n with 2 #ones'
    dn, total, factorial = float(n), float(S), float(F)
    if mean_free and dn*factorial <= TWO52 and total*total <= TWO52:
        return ('finite' if dn*factorial > total*total else 'poisson'), 'exact integer path'
    if not mean_free and mean == math.floor(mean) and dn*mean*mean+factorial <= TWO52 and 2*mean*total <= TWO52:
        return ('finite' if dn*mean*mean+factorial > 2*mean*total else 'poisson'), 'exact integer path'
    f = factorial/dn
    if mean > 4*average or mean < f/(4*average): return 'finite', 'padded path (far from the face)'
    edge = float(LD(mean)-2*LD(average)+LD(f)/LD(mean))
    pad = 256*EPS*(mean+2*average+f/mean)
    return ('finite' if edge > pad else 'poisson' if edge < -pad else 'unresolved'), 'padded path, edge %r, pad %.3g' % (edge, pad)

def replay_pig(r, data, budget=None):
    kind, _, _, _ = outcome(r); mask = r['mask']
    if mask == 3: return [], 'evaluation only'
    if not judged(r): return [], None
    x = ints(data); n = len(x); S = sum(x); Q = sum(v*v for v in x); F = Q-S
    if mask == 2:
        if S == 0:
            if r['held']['shape'] == 0:
                # shape 0 is the zero-mass member for every mean (flat-maximum records, zero-unique 72523133)
                return [support(r, {'estimate'}, 'all zero with shape held at 0: likelihood one for every mean', 'pig README existence and Zero member'),
                        unique(r, False, 'all zero with shape held at 0: mean unidentified', 'pig README existence and Zero member')], None
            return [support(r, {'limit:pois'}, 'all zero with positive shape held: unattained mean-zero supremum', 'pig README existence')], None
        return [support(r, {'estimate'}, 'some positive count with shape held: unique mean maximum', 'pig README existence')], None
    if S == 0:
        if mask == 0:
            return [unique(r, False, 'all zero, both free: (mu, 0) attains the maximum for every mu', 'pig README existence and Zero member')], None
        e = estimates(r)
        return [comparison('support', 'outcome %s, shape %s' % (label(r), e.get('shape')), decide(kind == 'estimate' and e.get('shape') == 0, r),
                           'all zero with mean held: attained zero mass, shape 0', note='source: pig README existence')], None
    average = float(Fraction(S, n))
    if mask == 0:
        value = n*F-S*S; truth = 'finite' if value > 0 else 'poisson'
        decision, path = pig_mirror(n, S, F, average, average, True)
        hp = 'n F - S^2 = %d (population variance - mean = %s)' % (value, exact(Fraction(value, n*n)))
        claim_text = '(variance - mean %s 0)'
    else:
        mu = Fraction(r['held']['mean'])
        value = n*mu*mu+F-2*mu*S; truth = 'finite' if value > 0 else 'poisson'
        ones = x.count(1) if max(x) == 1 else None
        decision, path = pig_mirror(n, S, F, float(mu), average, False, ones)
        hp = 'n mu^2 + F - 2 mu S = %s; relative gap %s' % (exact(value), gap(n*mu*mu+F, 2*mu*S))
        claim_text = '(n mu^2 + F - 2 mu S %s 0)'
    claim = 'poisson' if kind == 'limit' else 'finite'
    return [comparison('moment', '%s %s' % (claim, claim_text % ('<=' if claim == 'poisson' else '>')),
                       decide(truth == claim, r), hp, double='%s decides %s%s' % (path, decision, ' (DISAGREES with exact)' if decision not in (truth, 'unresolved') else ''),
                       margin='exact path: none; padded path: 256 eps (mean + 2 xbar + F/(n mean))',
                       note='mirror uses correctly rounded sums for the core\'s compensated ones')], None

# ---------------------------------------------------------------- foldnorm

def foldnorm_h0_mirror(x, extended):
    """Mirror of face_score(y, m2, 0, 0, 0).value on y = x/max(x)."""
    n = len(x); scale = max(x); y = [v/scale for v in x]; m2 = 0.0
    for v in y: m2 += v*v/n
    T = LD if extended else float
    fourth = T(0)
    for z in y:
        z2 = T(z)*T(z)/T(m2); fourth += z2*z2/T(n)
    return float(T(1)-fourth/T(3))

def replay_foldnorm(r, data, budget=None):
    kind, _, _, _ = outcome(r); mask = r['mask']
    if mask == 3: return [], 'evaluation only'
    if not judged(r): return [], None
    x = [float(v) for v in data]; fx = [Fraction(v) for v in x]; n = len(x)
    if mask == 0:
        if len(set(x)) == 1:
            return [support(r, {'nonexistence:unbounded'}, 'constant sample', 'foldnorm README existence')], None
        if kind != 'estimate': return [support(r, {'estimate'}, 'nonconstant sample: a finite maximum exists', 'foldnorm README existence')], None
        e = estimates(r)
        if e.get('mu') != 0: return [], 'mu > 0: H(0) does not decide (information only)'
        Q2 = sum(v*v for v in fx); Q4 = sum(v**4 for v in fx)
        h0 = 1-n*Q4/(3*Q2*Q2)
        positive = [v for v in fx if v > 0]
        exception = len(set(positive)) == 1 and 3*len(positive) <= n
        ok = h0 < 0 or (h0 == 0 and exception)
        verdict = 'holds' if ok else ('open' if h0 == 0 else decide(False, r))
        mirrors = ', '.join('%s %r' % (name, foldnorm_h0_mirror(x, ext)) for name, ext in (('long double', True), ('double', False)))
        return [comparison('quartic', 'estimate mu = 0 (half-normal boundary maximum: H(0) <= 0)', verdict,
                           'H(0) = %s%s' % (exact(h0), ' ({0, c} exception applies)' if exception else ''),
                           double='H(0) mirror: %s' % mirrors, margin='32 eps',
                           note='H(0) > 0 makes mu = 0 a local minimum of the profile; source: foldnorm README (half-normal face)')], None
    if mask == 1:
        mu = Fraction(r['held']['mu'])
        if all(v == mu for v in fx):
            return [support(r, {'nonexistence:unbounded'}, 'all observations equal the held mu', 'foldnorm README existence')], None
        return [support(r, {'estimate'}, 'some observation differs from the held mu', 'foldnorm README existence')], None
    sigma = Fraction(r['held']['sigma'])
    if kind != 'estimate': return [support(r, {'estimate'}, 'sigma held: the location maximum exists', 'foldnorm README existence')], None
    Q2 = sum(v*v for v in fx); value = Q2-n*sigma*sigma
    truth = 'positive' if value > 0 else 'zero'
    mu = estimates(r).get('mu'); claim = 'positive' if mu > 0 else 'zero'
    scale = max(max(x), float(sigma)); m2 = 0.0
    for v in x: y = v/scale; m2 += y*y/n
    s = float(sigma)/scale; decision = 'positive' if m2 > s*s else 'zero'
    return [comparison('location', 'mu %s (mean(x^2) - sigma^2 %s 0)' % ('> 0' if claim == 'positive' else '= 0', '>' if claim == 'positive' else '<='),
                       decide(truth == claim, r), 'mean(x^2) - sigma^2 = %s; relative gap %s' % (approx(value/n, 6), gap(Q2/n, sigma*sigma)),
                       double='m2 = %r, s^2 = %r (normalized), decides %s%s' % (m2, s*s, decision, ' (DISAGREES with exact)' if decision != truth else ''),
                       margin='none (unpadded)', note='source: foldnorm README existence (held scale)')], None

# ---------------------------------------------------------------- bbinom

def replay_bbinom(r, data, budget=None):
    kind, _, _, _ = outcome(r); mask = r['mask']
    if mask in (6, 7): return [], 'shapes held: size enumeration or evaluation'
    if not judged(r): return [], None
    x = ints(data); n = len(x)
    if mask == 0:          # nothing held: fitted only for all-zero counts (flat-count, 89b214ed)
        if max(x) == 0:
            return [unique(r, False, 'all-zero counts with nothing held: size 0 gives likelihood one for every shape pair',
                           'bbinom README; mle.hpp all-free zero branch')], None
        return [], 'nothing held: nonzero counts are unsupported'
    if mask in (2, 4):     # size free, one shape held
        if max(x) == 0:
            return [unique(r, False, 'all-zero counts with free size: likelihood one for every positive free shape',
                           'bbinom README (last paragraph); mle.hpp Unidentified branch')], None
        if min(x) == max(x):
            return [support(r, ('a point-mass claim', lambda g: g is not None and g.startswith('point-mass')),
                            'constant positive counts', 'bbinom mle.hpp free-size branch (PointMass)')], None
        return [support(r, NOT_DEGENERATE, 'nonconstant counts', 'bbinom mle.hpp free-size branch')], None
    N = int(r['held']['size'])
    if N == 0:
        return [unique(r, False, 'size 0 held: likelihood one for every positive free shape',
                       'bbinom README (last paragraph); mle.hpp Unidentified branch')], None
    n0, nN = x.count(0), x.count(N)
    if n0 == n or nN == n:
        return [support(r, {'point-mass@%d' % (0 if n0 == n else N)}, 'all observations at one end', 'bbinom mle.hpp (PointMass)')], None
    if mask != 1: return [support(r, NOT_DEGENERATE, 'not all at one end', 'bbinom mle.hpp')], None
    if N == 1: return [], 'size 1: unidentified'
    if n0+nN == n:
        return [support(r, {'two-point@0,%d' % N}, 'every observation 0 or size, both present', 'bbinom mle.hpp (TwoPointLimit)')], None
    if N != 2: return [support(r, NOT_DEGENERATE, 'interior observations present', 'bbinom mle.hpp')], None
    n1, n2 = x.count(1), x.count(2)
    D = 4*n0*n2-n1*n1
    truth = 'finite' if D > 0 else 'face'
    got = label(r); claim = 'finite' if got == 'estimate' else 'face' if got == 'limit:binom' else got
    comps = [comparison('size2', 'outcome %s' % got, decide(truth == claim, r),
                        '4 n0 n2 - n1^2 = %d (n0, n1, n2 = %d, %d, %d): %s' % (D, n0, n1, n2,
                            'finite closed-form MLE' if D > 0 else 'binomial face (frequencies at or beyond binomial dispersion)'),
                        double='integer counts: exact in binary64 (core margin 64 eps (left + right))', margin='64 eps (4 n0 n2 + n1^2)',
                        note='the N = 2 cell probabilities satisfy P1^2 < 4 P0 P2 for finite shapes, = for the binomial; '
                             'source: bbinom mle.hpp N == 2 branch')]
    if D > 0 and kind == 'estimate':
        alpha, beta = Fraction(n1+2*n2)*n1/D, Fraction(2*n0+n1)*n1/D
        e = estimates(r)
        err = max(abs(Fraction(e['alpha'])-alpha)/alpha, abs(Fraction(e['beta'])-beta)/beta)
        comps.append(comparison('closed-form', 'alpha %r, beta %r' % (e['alpha'], e['beta']), decide(err <= Fraction(1, 10**10), r),
                                'alpha = %s, beta = %s (exact); relative error %s against tol 1e-10' % (exact(alpha), exact(beta), approx(err, 3))))
    return comps, None

# ---------------------------------------------------------------- cauchy, slash, hcauchy

def multiplicity_expectation(k, n):
    return {'estimate'} if 2*k < n else {'nonexistence:unattained'} if 2*k == n else {'nonexistence:unbounded'}

def cauchy_mirror(x, extended, depth=0):
    """Mirror of fit_cauchy's exact data tests, including the rescaled recursion."""
    data = sorted(x); n = len(data)
    if n < 2: return 'nonexistence (n < 2)'
    if n % 2 == 0 and data[0] != data[-1] and data[n//2-1] == data[0] and data[n//2] == data[-1]:
        return 'balanced two-point estimate'
    run = 1
    for i in range(1, n):
        run = run+1 if data[i] == data[i-1] else 1
        if 2*run >= n: return 'nonexistence (run %d)' % run
    half = data[-1]/2-data[0]/2; centre = data[0]/2+data[-1]/2
    if depth == 0 and half > 0 and math.isfinite(half) and (half < 1e-100 or half > 1e100 or abs(centre) > 1e6*half):
        T = LD if extended else float
        y = [float((T(v)-T(centre))/T(half)) for v in x]
        return 'rescaled: ' + cauchy_mirror(y, extended, 1)
    return 'search'

def replay_cauchy(r, data, budget=None):
    kind, _, _, _ = outcome(r); mask = r['mask']
    if mask in (2, 3): return [], 'scale held: location search (no face inequality)' if mask == 2 else 'evaluation only'
    if not judged(r): return [], None
    x = [float(v) for v in data]; n = len(x); counts = collections.Counter(x)
    if mask == 1:
        loc = r['held']['location']; k = counts.get(loc, 0)
        return [support(r, multiplicity_expectation(k, n), 'k = #(x == location) = %d, n = %d' % (k, n),
                        'cauchy README existence and reason (collapse ~ sigma^(n-2k))', name='multiplicity')], None
    k = max(counts.values())
    mirror = '; '.join('%s: %s' % (name, cauchy_mirror(x, ext)) for name, ext in (('long double', True), ('double', False)))
    if len(counts) == 2 and n % 2 == 0 and k*2 == n:
        return [support(r, {'estimate'}, 'balanced two-point sample', 'cauchy README existence', name='multiplicity', note='core mirror: '+mirror),
                unique(r, False, 'balanced two-point sample: a continuum of maximizing pairs', 'cauchy README existence; mle.hpp comment')], None
    why = 'largest multiplicity k = %d, n = %d' % (k, n)
    return [support(r, multiplicity_expectation(k, n), why, 'cauchy README existence and reason (2k = n: finite collapse supremum)',
                    name='multiplicity', note='core mirror: '+mirror)], None

def slash_mirror(x, extended):
    """Mirror of slash_mle's free multiplicity test on the scaled data."""
    T = LD if extended else float
    y = sorted(x); n = len(y); centre = y[n//2]
    distances = sorted(float(abs(T(v)-T(centre))) for v in y); scale = distances[n//2]
    if not (scale > 0) or not math.isfinite(scale): scale = y[-1]/2-y[0]/2
    if not (scale > 0): return 'unbounded (zero scale)', None
    z = [float((T(v)-T(centre))/T(scale)) for v in y]
    k = max(collections.Counter(z).values())
    return ('unbounded' if 2*k > n else 'half multiplicity (margin comparison)' if 2*k == n else 'search'), k

def replay_slash(r, data, budget=None):
    kind, _, _, _ = outcome(r); mask = r['mask']
    if mask in (2, 3): return [], 'sigma held: location search (no face inequality)' if mask == 2 else 'evaluation only'
    if not judged(r): return [], None
    x = [float(v) for v in data]; n = len(x); counts = collections.Counter(x)
    if mask == 1:
        mu = r['held']['mu']; k = counts.get(mu, 0)
        return [support(r, multiplicity_expectation(k, n), 'k = #(x == mu) = %d, n = %d' % (k, n),
                        'slash README existence', name='multiplicity')], None
    k = max(counts.values())
    expected = {'estimate', 'nonexistence:unattained'} if 2*k == n else multiplicity_expectation(k, n)
    mirrors = []
    for name, ext in (('long double', True), ('double', False)):
        decision, km = slash_mirror(x, ext)
        mirrors.append('%s: %s%s' % (name, decision, '' if km in (None, k) else ' (scaled multiplicity %d, exact %d)' % (km, k)))
    return [support(r, expected, 'largest multiplicity k = %d, n = %d%s' % (k, n, ' (half: decided by the 1e-8 n margin, not replayed)' if 2*k == n else ''),
                    'slash README existence', name='multiplicity', note='core mirror on the scaled data: '+'; '.join(mirrors))], None

def replay_hcauchy(r, data, budget=None):
    kind, _, _, _ = outcome(r); mask = r['mask']
    if mask in (2, 3): return [], 'sigma held: endpoint is the minimum' if mask == 2 else 'evaluation only'
    if not judged(r): return [], None
    x = [float(v) for v in data]; n = len(x)
    mu = min(x) if mask == 0 else r['held']['mu']
    k = sum(1 for v in x if v == mu)
    return [support(r, multiplicity_expectation(k, n), 'k = #(x == %s) = %d, n = %d' % ('min x' if mask == 0 else 'mu', k, n),
                    'hcauchy README existence', name='multiplicity',
                    note='binary64 x - mu is zero exactly when x == mu (gradual underflow): the core count is exact')], None

# ---------------------------------------------------------------- sgomp

def replay_sgomp(r, data, budget=None):
    kind, _, _, _ = outcome(r); mask = r['mask']
    if mask == 3: return [], 'evaluation only'
    if not judged(r): return [], None
    x = [float(v) for v in data]
    if mask == 0:
        if max(x) == 0 or len(set(x)) == 1:
            return [support(r, {'nonexistence:unbounded'}, 'constant or all-zero sample', 'sgomp README existence')], None
        return [support(r, {'estimate'}, 'nonconstant sample: a finite MLE exists', 'sgomp README existence')], None
    if mask == 2:
        if max(x) == 0: return [support(r, {'nonexistence:unbounded'}, 'all zero with b free', 'sgomp README existence; mle.hpp Collapse')], None
        return [support(r, {'estimate'}, 'positive sample sum: unique rate maximum', 'sgomp README existence')], None
    if kind != 'estimate': return [support(r, {'estimate'}, 'b held: unique eta estimate', 'sgomp README existence')], None
    b = r['held']['b']
    score = sum((1-2*iv.exp(-iv.mpf(b)*iv.mpf(v)) for v in x), iv.mpf(0))
    side = interval_side(score, 0)
    eta = estimates(r).get('eta')
    claim = 'zero' if eta == 0 else 'positive'
    truth = {1: 'positive', -1: 'zero', 0: None}[side]
    verdict = 'open' if truth is None else decide(truth == claim, r)
    scale = max(x) or 1.0; bs = b*scale; face = 0.0
    for v in x: y = v/scale; face += -math.expm1(-bs*y)-math.exp(-bs*y)
    decision = 'zero' if face <= 0 else 'positive'
    return [comparison('face-score', 'eta %s (score at eta = 0 %s 0)' % ('= 0' if claim == 'zero' else '> 0', '<=' if claim == 'zero' else '>'),
                       verdict, 'sum(1 - 2 exp(-b x)) in %s' % ('[%s, %s]' % (mp.nstr(mp.mpf(score.a), 12), mp.nstr(mp.mpf(score.b), 12))),
                       double='face_score = %r, decides %s%s' % (face, decision, ' (DISAGREES with exact)' if truth and decision != truth else ''),
                       margin='none (unpadded)', rigor='interval', note='source: sgomp README reason; mle.hpp profile/face_score')], None

# ---------------------------------------------------------------- discrete point-mass and two-point predicates

def one(expected, why, source): return expected, why, source

def discrete(family):
    def replay(r, data, budget=None):
        if not judged(r): return [], None
        x = ints(data); mask = r['mask']
        got = RULES[family](r, x, mask)
        if got is None: return [], 'no exact support predicate for this hold subset'
        comps = []
        for item in got:
            if item[0] == 'unique': comps.append(unique(r, item[1], item[2], item[3]))
            else: comps.append(support(r, item[0], item[1], item[2]))
        return comps, None
    return replay

def rule_dnorm(r, x, mask):
    U = sorted(set(x)); src = 'dnorm README existence; mle.hpp'
    if mask == 0:
        if len(U) == 1: return [one({'point-mass@%d' % U[0]}, 'constant sample', src)]
        if U[-1]-U[0] == 1: return [one({'two-point@%d,%d' % (U[0], U[-1])}, 'two adjacent integers', src)]
        return [one({'estimate'}, 'integer range %d >= 2: unique finite MLE' % (U[-1]-U[0]), src)]
    if mask == 1:
        mu = Fraction(r['held']['mean'])
        if all(v <= mu <= v+1 for v in x):
            lo = math.floor(mu)
            e = {'two-point@%d,%d' % (lo-1, lo)} if mu == lo else {'point-mass@%d' % lo}
            return [one(e, 'every cell closure [x, x+1] contains the held mean', src)]
        return [one({'estimate'}, 'some cell closure misses the held mean', src)]
    if mask == 2: return [one({'estimate'}, 'sd held: location maximum exists', src)]
    return None

def rule_dweibull(r, x, mask):
    U = sorted(set(x)); src = 'dweibull README existence; mle.hpp'
    if mask == 0:
        if U[-1] == 0 or len(U) == 1: return [one({'point-mass@%d' % U[0]}, 'constant sample', src)]
        if U[-1]-U[0] == 1: return [one({'two-point@%d,%d' % (U[0], U[-1])}, 'two adjacent counts', src)]
        return [one({'estimate'}, 'range >= 2: unique finite MLE', src)]
    if mask == 1:
        if U[-1] == 0: return [one({'estimate'}, 'shape1 held, all zero: every shape2 maximizes', src),
                               ('unique', False, 'shape1 held, all zero', src)]
        if U[-1] == 1: return [one({'two-point@0,1'}, 'shape1 held, counts in {0, 1}', src)]
        return [one({'estimate'}, 'shape1 held, some count >= 2', src)]
    if mask == 2:
        if U[-1] == 0: return [one({'point-mass@0'}, 'shape2 held, all zero', src)]
        return [one({'estimate'}, 'shape2 held, some positive count', src)]
    return None

def rule_dgamma(r, x, mask):
    U = sorted(set(x)); src = 'dgamma README existence; mle.hpp'
    if mask == 0:
        if len(U) == 1: return [one({'point-mass@%d' % U[0]}, 'constant sample', src)]
        if U[-1]-U[0] == 1: return [one({'two-point@%d,%d' % (U[0], U[-1])}, 'two adjacent integers', src)]
        return [one({'estimate'}, 'range >= 2: a finite MLE exists', src)]
    if mask in (1, 2):
        if U[-1] == 0: return [one({'point-mass@0'}, 'one coefficient held, all zero', src)]
        return [one({'estimate'}, 'one coefficient held, some positive count', src)]
    return None

def rule_dlaplace(r, x, mask):
    y = sorted(x); n = len(y); src = 'dlaplace README existence; mle.hpp'
    single = y[(n-1)//2] == y[n//2]
    if mask == 0:
        if len(set(y)) == 1: return [one({'point-mass@%d' % x[0]}, 'constant sample', src)]
        return [one({'estimate'}, 'nonconstant sample', src), ('unique', single, 'median interval [%d, %d]' % (y[(n-1)//2], y[n//2]), src)]
    if mask == 1:
        loc = r['held']['location']
        if all(v == loc for v in x): return [one({'point-mass@%d' % x[0]}, 'all observations equal the held location', src)]
        return [one({'estimate'}, 'some observation differs from the held location', src)]
    if mask == 2:
        return [one({'estimate'}, 'scale held', src), ('unique', single, 'median interval [%d, %d]' % (y[(n-1)//2], y[n//2]), src)]
    return None

def rule_cmp(r, x, mask):
    U = sorted(set(x)); src = 'cmp README existence; special/cmp.hpp cmp_mle_work'
    if mask == 0:
        if U[-1] == 0: return [one({'estimate'}, 'all zero: lambda = 0 attains the maximum', src),
                               ('unique', False, 'all zero, both free: nu unidentified', src)]
        if len(U) == 1: return [one({'point-mass@%d' % U[0]}, 'positive constant sample', src)]
        if U[-1]-U[0] == 1: return [one({'two-point@%d,%d' % (U[0], U[-1])}, 'two adjacent integers', src)]
        return [one({'estimate'}, 'other samples: unique finite maximum (possibly nu = 0)', src)]
    if mask == 1:
        if r['held']['lambda'] == 0:
            # lambda = 0 is the point mass at 0 for every nu (flat-maximum records, zero-unique 72523133)
            if U[-1] == 0: return [one({'estimate'}, 'lambda held at 0, all zero: likelihood one for every nu', src),
                                   ('unique', False, 'lambda held at 0, all zero: nu unidentified', src)]
            return None
        if U[-1] <= 1: return [one({'two-point@0,1'}, 'lambda held, counts in {0, 1}: supremum as nu -> inf', src)]
        return [one({'estimate'}, 'lambda held, some count > 1', src)]
    if mask == 2: return [one({'estimate'}, 'nu held: the rate maximum exists', src)]
    return None

def rule_cmb(r, x, mask):
    U = sorted(set(x)); n = len(x); S = sum(x); src = 'cmb README existence; mle.hpp'
    if mask in (1, 3):
        N = int(r['held']['size'])
        if N <= 1: return None
        ends = all(v in (0, N) for v in x)
        if mask == 1:
            if S == 0 or S == n*N: return [one({'estimate'}, 'all at an end: endpoint probability attained', src)]
            if len(U) == 1: return [one({'point-mass@%d' % U[0]}, 'constant interior sample', src)]
            if U[-1]-U[0] == 1: return [one({'two-point@%d,%d' % (U[0], U[-1])}, 'two adjacent integers', src)]
            if ends: return [one({'two-point@0,%d' % N}, 'every observation 0 or size', src)]
            return [one(NOT_DEGENERATE, 'other samples', src)]
        p = r['held']['prob']
        if p in (0, 1): return None
        if ends: return [one({'two-point@0,%d' % N}, 'prob held, every observation 0 or size', src)]
        if all(v in (N//2, (N+1)//2) for v in x):
            e = {'point-mass@%d' % (N//2)} if N % 2 == 0 else {'two-point@%d,%d' % (N//2, N//2+1)}
            return [one(e, 'prob held, every observation at the centre', src)]
        return [one(NOT_DEGENERATE, 'other samples', src)]
    if mask == 0:
        mn, mx = U[0], U[-1]
        if mn == mx: return None
        counts = collections.Counter(x)
        uniform = mn == 0 and len(U) == mx+1 and len(set(counts.values())) == 1
        if uniform: return None
        if mx-mn == 1 and mn > 0: return [one({'two-point@%d,%d' % (mn, mx)}, 'two adjacent positive integers', src)]
        if mn == 0 and mx >= 2 and all(v in (0, mx) for v in x): return [one({'two-point@0,%d' % mx}, 'observations on 0 and one integer >= 2', src)]
        if mx-mn == 1: return None
        return [one(NOT_DEGENERATE, 'other samples', src)]
    return None

def rule_point_at_one(name):
    def rule(r, x, mask):
        if mask != 0: return None
        src = '%s README existence; mle.hpp' % name
        if all(v == 1 for v in x): return [one({'point-mass@1'}, 'every observation equals one', src)]
        return [one({'estimate'}, 'some observation exceeds one', src)]
    return rule

def rule_zipf(r, x, mask):
    if mask & 2: return None
    N = int(r['held']['N']) if mask & 1 else max(x); src = 'zeta truncation.hpp zeta_window_mle; modifiers.hpp'
    if N == 1: return [one({'estimate'}, 'window {1}', src), ('unique', False, 'window {1}: the exponent is unidentified', src)]
    if all(v == 1 for v in x): return [one({'point-mass@1'}, 'all at the lower window end', src)]
    if all(v == N for v in x): return [one({'point-mass@%d' % N}, 'all at the upper window end', src)]
    return [one({'estimate'}, 'observations off the window ends', src)]

RULES = {'dnorm': rule_dnorm, 'dweibull': rule_dweibull, 'dgamma': rule_dgamma, 'dlaplace': rule_dlaplace,
         'cmp': rule_cmp, 'cmb': rule_cmb, 'lgser': rule_point_at_one('lgser'), 'zeta': rule_point_at_one('zeta'),
         'yulesimon': rule_point_at_one('yulesimon'), 'zeta +truncate=zipf': rule_zipf}

# ---------------------------------------------------------------- zip (pois zero-inflated route)

def replay_zip(r, data, budget=None):
    kind, _, _, _ = outcome(r); mask = r['mask']
    if mask in (1, 3): return [], 'lambda held: closed form or evaluation'
    if not judged(r): return [], None
    x = ints(data); n = len(x); S = sum(x); n0 = x.count(0)
    if mask == 2:
        if S == 0 and r['held']['pi'] < 1:
            return [support(r, {'limit:pois'}, 'pi held below one, all zero: the rate tends to zero', 'pois README modifiers (inflated)')], None
        return [], 'pi held: held-rate certificate (not mechanism D)'
    if S == 0:
        return [unique(r, False, 'all zero: pi = 1 with any lambda attains the maximum', 'pois/zip_mle.hpp comment ("Non-identifiable")')], None
    zero_fraction = Fraction(n0, n)
    value = ivq(zero_fraction)-iv.exp(-ivq(Fraction(S, n)))
    side = interval_side(value, 0)
    truth = {1: 'inflated', -1: 'poisson', 0: None}[side]
    pi = estimates(r).get('pi'); claim = 'inflated' if pi > 0 else 'poisson'
    sx = 0.0; zeros = 0.0
    for v in x: zeros += v == 0; sx += float(v)
    decision = 'inflated' if zeros/n > math.exp(-(sx/n)) else 'poisson'
    return [comparison('zero-fraction', 'pi %s (zero fraction - exp(-mean) %s 0)' % ('> 0' if claim == 'inflated' else '= 0', '>' if claim == 'inflated' else '<='),
                       'open' if truth is None else decide(truth == claim, r),
                       '#zeros/n - exp(-mean) in [%s, %s]' % (mp.nstr(mp.mpf(value.a), 12), mp.nstr(mp.mpf(value.b), 12)),
                       double='r0 = %r, exp(-x_bar) = %r, decides %s%s' % (zeros/n, math.exp(-(sx/n)), decision, ' (DISAGREES with exact)' if truth and decision != truth else ''),
                       margin='none (unpadded)', rigor='interval', note='source: pois/zip_mle.hpp (W_0 root iff the zero rate exceeds exp(-mean))')], None

# ---------------------------------------------------------------- tnorm (norm truncate route)

def truncated_exponential_variance(m):
    """Variance on (0, 1) of the density proportional to exp(tau u) whose mean is m (0 < m < 1), as an interval.

    The mean 1/(1 - exp(-tau)) - 1/tau increases in tau; the variance
    h(tau) = 1/tau^2 - 1/(4 sinh(tau/2)^2) decreases strictly in |tau| because
    h'(tau) has the sign of u^3 cosh u - sinh(u)^3 (u = tau/2), negative by
    Lazarevic's inequality (sinh u/u)^3 > cosh u. So h at the two ends of a
    bisected tau bracket encloses the variance; evaluating h on the bracket
    itself would lose all digits to cancellation when tau is small."""
    if m == Fraction(1, 2): return ivq(Fraction(1, 12)), 0
    if m < Fraction(1, 2): m = 1-m      # tau -> -tau leaves the variance unchanged
    saved = iv.dps; iv.dps = max(saved, 80)
    try:
        target = ivq(m)
        g = lambda t: 1/(1-iv.exp(-t))-1/t
        lo, hi = mp.mpf(0), mp.mpf(1)
        while interval_side(g(iv.mpf(hi))-target, 0) <= 0: hi *= 2
        for _ in range(600):
            mid = (lo+hi)/2
            side = interval_side(g(iv.mpf(mid))-target, 0)
            if side < 0: lo = mid
            elif side > 0: hi = mid
            else: break
            if hi-lo < mp.mpf(10)**(-60)*hi: break
        if lo == 0: return None, hi
        def h(t):
            e = iv.exp(iv.mpf(t)); t = iv.mpf(t)
            return 1/(t*t)-e/((e-1)*(e-1))
        return iv.mpf([h(hi).a, h(lo).b]), hi
    finally:
        iv.dps = saved

def replay_tnorm(r, data, budget=None):
    kind, _, _, _ = outcome(r); mask = r['mask']
    if not judged(r): return [], None
    x = [float(v) for v in data]; fx = [Fraction(v) for v in x]; n = len(x)
    held = r['held']
    a = held['a'] if mask & 4 else min(x); b = held['b'] if mask & 8 else max(x)
    src = 'norm/tnorm_mle.hpp header and face tests'
    estimated = not (mask & 4 and mask & 8)
    if (mask & 3) and len(set(x)) == 1 and estimated and (not mask & 4 or a == x[0]) and (not mask & 8 or b == x[0]):
        return [support(r, {'nonexistence:unbounded'}, 'constant sample, estimated bound collapses onto it', 'norm/tnorm_mle.hpp estimated_bounds_collapse')], None
    if mask & 3 == 2:
        if len(set(x)) == 1 and x[0] in (a, b):
            return [support(r, {'nonexistence:unbounded'}, 'sd held, constant data at a finite bound', src)], None
        return [], 'sd held: location solve (no face)'
    if mask & 3 == 3: return [], 'mean and sd held: evaluation'
    S = sum(fx); Q = sum(v*v for v in fx)
    if mask & 3 == 0:
        if len(set(x)) == 1: return [support(r, {'nonexistence:unbounded'}, 'constant sample', src)], None
        lower, upper = math.isfinite(a), math.isfinite(b)
        if not (lower or upper): return [], 'no truncation: normal closed form'
        v = (n*Q-S*S)/(n*n); xbar = S/n
        if lower and upper:
            w = Fraction(b)-Fraction(a)
            h, tau = truncated_exponential_variance((xbar-Fraction(a))/w)
            if h is None: return [comparison('window', 'outcome %s' % label(r), 'open', 'rate of the face not resolved', rigor='interval')], None
            face = ivq(w*w)*h if not isinstance(h, Fraction) else ivq(w*w*h)
            side = interval_side(face-ivq(v), 0)          # face variance - sample variance
            truth = {1: 'interior', -1: 'face', 0: None}[side]
            hp = 'truncated-exponential face variance - population variance in [%s, %s] (face rate tau/(b-a), |tau| <= %s)' % (
                mp.nstr(mp.mpf((face-ivq(v)).a), 10), mp.nstr(mp.mpf((face-ivq(v)).b), 10), mp.nstr(tau, 6))
            rigor = 'interval'; faces = {'limit:unavailable:truncated-exponential'}
        else:
            vf = (xbar-Fraction(a))**2 if lower else (Fraction(b)-xbar)**2
            truth = 'interior' if vf > v else 'face'
            hp = '(%s)^2 - population variance = %s; relative gap %s' % ('mean - a' if lower else 'b - mean', exact(vf-v), gap(vf, v))
            rigor = 'exact'; faces = {'limit:unavailable:translated-exponential'} if lower else {'limit:unavailable:reflected-exponential'}
    else:
        mu = Fraction(held['mean'])
        if all(v == mu for v in fx): return [support(r, {'nonexistence:unbounded'}, 'all observations equal the held mean', src)], None
        if not (math.isfinite(a) and math.isfinite(b)): return [], 'mean held, one-sided window: no face'
        A, B = Fraction(a)-mu, Fraction(b)-mu
        vf = (A*A+A*B+B*B)/3; m2 = sum((v-mu)**2 for v in fx)/n
        truth = 'interior' if vf > m2 else 'face'
        hp = 'uniform face mean((u-mu)^2) - mean((x-mu)^2) = %s; relative gap %s' % (exact(vf-m2), gap(vf, m2))
        rigor = 'exact'; faces = {'limit:unif'}
    got = label(r)
    claim = 'interior' if got == 'estimate' else 'face' if got in faces else got
    if truth is None:
        return [comparison('window', 'outcome %s' % got, 'open', hp, rigor=rigor, note='source: '+src)], None
    note = 'source: %s; window [%r, %r]%s' % (src, a, b, '; a face claim against an interior predicate is decided by the 1e-8 n margin' if claim == 'face' and truth == 'interior' else '')
    return [comparison('window', 'outcome %s (%s)' % (got, claim), decide(truth == claim, r), hp, rigor=rigor, note=note)], None

# ---------------------------------------------------------------- registry

REPLAYS = {'binom': replay_binom, 'gompertz': replay_gompertz, 'pig': replay_pig, 'foldnorm': replay_foldnorm,
           'bbinom': replay_bbinom, 'cauchy': replay_cauchy, 'slash': replay_slash, 'hcauchy': replay_hcauchy,
           'sgomp': replay_sgomp, 'pois+zero=inflated': replay_zip, 'norm +truncate=tnorm': replay_tnorm}
REPLAYS.update({family: discrete(family) for family in RULES})
