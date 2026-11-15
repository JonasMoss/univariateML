#!/usr/bin/env python3
"""Strict layer: replay certificate comparisons in high precision (step 8 prototype).

The package standard for a certified outcome is "certified in exact
arithmetic, conditional on the computed comparisons, with a stated practical
floating-point margin". This script checks the condition on stored snapshots
(fit-snapshot.py): for every case whose outcome rests on a certificate, the
comparisons that decide the outcome are re-derived from the returned
coefficients, the data and the dossier mathematics, and evaluated in mpmath
interval arithmetic (mp.iv at DPS digits, outward rounded) or in exact
rational arithmetic. Nothing is read from the core beyond the snapshot record;
see dev/design/certificate-replay.md for the design, the comparisons each
family's certificate makes, and what a trace hook in the core would add.

A comparison HOLDS when its enclosure lies strictly on the side the outcome
claims, FAILS when it lies strictly on the other side, and is OPEN otherwise
(enclosure contains the threshold, a work cap, or no rigorous evaluation). A
failure is a certificate whose condition is false for that input; an open
comparison is listed, not judged. Where the core's own double evaluation can
be mirrored in Python (binary64 arithmetic, the core's compensated sums and
margins), the record also gives that double value and the core's margin at the
same point; the mirror follows master 7f12c88d and is evidence, not the core.
The claimed widths and margins below are read from the code at 7f12c88d; a
change to either certificate must update this script in the same commit.

Families and comparisons (dossier mathematics in each family README.md):

  nbinom  dispersion  all free: Poisson limit iff n*sum(x^2) <= (sum x)^2 + n*sum(x)
                      (population variance <= mean), exact integer arithmetic
          root        all free or prob held: the profile score
                      g(r) = mean_i sum_{j<x_i} r/(r+j) + T(r),  T = -r log1p(xbar/r)
                      or r log(prob), changes sign on [r(1-w), r(1+w)], w = 5e-7
                      (the certificate's bisection width; the returned size lies
                      in the signed bracket)
  lomax   gain        all free, estimate: per-observation gain over the best
                      exponential h(z) = z - log A(z) - A(z) exceeds 1e-8
          root        all free, estimate: h'(z) = 1 - B/A - B changes sign on
                      z_hat -/+ 8e-7 (the certificate's enclosure: narrow
                      bracket below 7e-7 widened by 1e-7 on each side)
          concave     all free, estimate: the termwise curvature bound
                      B_r^2/A_l^2 - C_low/A_r - C_low < 0 on that enclosure
          global      all free, estimate: interval branch and bound over the
                      certificate's compact domain [lo, hi] minus the enclosure
                      (chord bound with the dossier's curvature bound, score-sign
                      exclusion) and both analytic tails stay below h(z_hat)
          face        all free, exponential limit (certified = false): the same
                      branch and bound shows h < 1e-8 everywhere, i.e. no gain
                      above the margin; the record gives the best gain found
          root        kappa held: F(t) = n - (kappa+1) sum logistic(t + log x_i)
                      changes sign on log(lambda) -/+ 0.75e-10 (half-tol bracket
                      plus quarter-tol restoration)

Held coordinates with a closed form (nbinom size held, lomax lambda held) and
all-held evaluations make no comparison and are counted, not judged.

Usage:
  cert-replay.py NATIVE [DOUBLE] [--family lomax,nbinom] [--records FILE]
                 [--dps 50] [--budget 4000] [--prune | --add REF [--match RE]]
NATIVE and DOUBLE are as for fit-judge.py. Failures are keyed
'cert-replay:<comparison>|mode|family|source|case' and compared with
strict-known.json. Reserve one slot: /tmp/umvl-cpu/run python3 ...
"""
import argparse, collections, gzip, hashlib, heapq, json, math, re, sys, time
from fractions import Fraction
import mpmath as mp
from strict_common import add_arguments, load, mode_of, number, ratchet, snapshot_module

CHECK = 'cert-replay'
EPS = 2.0**-52
TOL = 1e-10               # every snapshot fit uses default controls
fs = snapshot_module()
iv = mp.iv

# ---------------------------------------------------------------- records

def comparison(name, claim, verdict, hp, double=None, margin=None, rigor='interval', note='', info=None):
    return dict(name=name, claim=claim, verdict=verdict, hp=hp, double=double, margin=margin,
                rigor=rigor, note=note, info=info)

def show(v, digits=20):
    """Interval or number as text; an interval prints as [lo, hi]."""
    if v is None: return None
    if isinstance(v, iv.mpf):
        return '[%s, %s]' % (mp.nstr(mp.mpf(v.a), digits), mp.nstr(mp.mpf(v.b), digits))
    if isinstance(v, (int, Fraction)) and not isinstance(v, bool): return str(v)
    if isinstance(v, float): return repr(v)
    return mp.nstr(v, digits)

def sign_verdict(value, side):
    """'holds' if the interval lies strictly on `side` (+1 above zero, -1 below)."""
    lo, hi = mp.mpf(value.a), mp.mpf(value.b)
    if side > 0: return 'holds' if lo > 0 else 'fails' if hi < 0 else 'open'
    return 'holds' if hi < 0 else 'fails' if lo > 0 else 'open'

def above(value, threshold):
    lo, hi = mp.mpf(value.a), mp.mpf(value.b)
    return 'holds' if lo > threshold else 'fails' if hi < threshold else 'open'

class Kahan:
    """The core's compensated sum (nbinom_cert_detail::Sum, lomax_cert_detail::Sum)."""
    def __init__(self): self.value = 0.0; self.correction = 0.0
    def add(self, x):
        y = x-self.correction; t = self.value+y
        self.correction = (t-self.value)-y; self.value = t

# ---------------------------------------------------------------- nbinom

def nbinom_core_dispersion(x):
    """Mirror of nbinom/certificate.hpp's dispersion guard (all free): decision, value, pad, path."""
    total = Kahan(); squares = 0.0
    for v in x: total.add(float(v)); squares += float(v)*v
    n = float(len(x)); mean = total.value/n
    centered = Kahan()
    for v in x: centered.add((v-mean)*(v-mean)/n)
    pad = 256*EPS*(1+centered.value+mean)*(1+n*EPS)
    exact = n*squares <= 4503599627370496. and total.value*total.value+n*total.value <= 4503599627370496.
    if exact:
        return ('poisson' if mean > 0 and n*squares <= total.value**2+n*total.value else 'finite',
                (n*squares-(total.value**2+n*total.value))/(n*n), 0.0, 'exact integer path')
    if abs(centered.value-mean) <= pad: return 'unresolved', centered.value-mean, pad, 'padded path'
    return ('poisson' if mean > 0 and centered.value < mean-pad else 'finite', centered.value-mean, pad, 'padded path')

def log1pmx(u):
    out, power = 0.0, u*u
    for k in range(2, 21):
        out += (1. if k % 2 else -1.)*power/k; power *= u
    return out

def nbinom_core_score(counts, n, mean, r, prob):
    """Mirror of the score lambda in nbinom/certificate.hpp: (r times mean score, error)."""
    residual = prob is None and r > 100.*max(counts)
    s, scale, rem = Kahan(), Kahan(), Kahan()
    for v in sorted(counts):
        h = e = 0.0
        if v <= 4096:
            acc = Kahan()
            for j in range(v): acc.add(-float(j)/(r+j) if residual else r/(r+j))
            h = acc.value
        elif r >= 16:
            u, w = 1/r, 1/(r+v); u2, w2 = u*u, w*w
            h = r*((log1pmx(v/r) if residual else math.log1p(v/r))+(u-w)/2+(u2-w2)/12-(u2*u2-w2*w2)/120+
                   (u2*u2*u2-w2*w2*w2)/252-(u**8-w**8)/240+(u**10-w**10)/132)
            e = r*691./32760*u**12
        else:   # Boost digamma in the core; mpmath rounded to binary64 here
            a, b = float(mp.digamma(r+v)), float(mp.digamma(r))
            h = r*(a-b); e = 128*EPS*r*(1+abs(a)+abs(b))
        weight = float(counts[v])/n
        s.add(weight*h); scale.add(weight*abs(h)); rem.add(weight*e)
    term = -r*log1pmx(mean/r) if residual else r*(-math.log1p(mean/r) if prob is None else math.log(prob))
    error = rem.value+128*EPS*((2.2250738585072014e-308 if residual else 1.)+scale.value+abs(term))*(1+len(counts)*EPS)
    return s.value+term, error

def nbinom_score_hp(counts, n, total, r, prob):
    """Interval enclosure of g(r) = mean_i sum_{j<x_i} r/(r+j) + T(r) at an exact point r."""
    r = iv.mpf(r)
    largest = max(counts)
    if largest <= 20000:
        acc = iv.mpf(0); running = iv.mpf(0); j = 0
        for v in sorted(counts):
            while j < v: running += r/(r+j); j += 1
            acc += counts[v]*running
        rigor = 'interval'
    else:   # psi difference in ordinary mpmath: numerical, not an enclosure
        rr = mp.mpf(r.a)
        acc = iv.mpf(mp.fsum(c*rr*(mp.digamma(rr+v)-mp.digamma(rr)) for v, c in counts.items()))
        rigor = 'numerical'
    mean = iv.mpf(total)/n
    term = -r*iv.log1p(mean/r) if prob is None else r*iv.log(iv.mpf(prob))
    return acc/n+term, rigor

def replay_nbinom(r, data, budget=None):
    out = []
    mask = r['mask']; res = r['result']; kind = (res.get('outcome') or {}).get('kind')
    x = [int(v) for v in data]
    if mask == 3 or not x: return out, 'evaluation only' if mask == 3 else 'empty'
    if mask == 1: return out, 'closed form (size held)'
    n = len(x); S = sum(x); Q = sum(v*v for v in x)
    if S == 0: return out, 'degenerate (all zero): no comparison'
    if mask == 0 and kind in ('estimate', 'limit'):
        exact = Fraction(n*Q-(S*S+n*S), n*n)   # population variance minus mean
        claim = 'poisson' if kind == 'limit' else 'finite'
        truth = 'poisson' if exact <= 0 else 'finite'
        decided, value, pad, path = nbinom_core_dispersion(x)
        out.append(comparison('dispersion', '%s (population variance - mean %s 0)' % (claim, '<=' if claim == 'poisson' else '>'),
                              'holds' if truth == claim else 'fails', '%s = %.17g' % (exact, float(exact)),
                              double=value, margin=pad, rigor='exact', note='core %s decides %s' % (path, decided)))
    if kind != 'estimate': return out, None
    size = number(res['estimates']['size'])
    prob = None if mask == 0 else r['held']['prob']
    if not (size > 0 and math.isfinite(size)): return out, 'size %r: no root comparison' % size
    counts = collections.Counter(x)
    w = 5e-7
    left, right = mp.mpf(size)*(1-mp.mpf(w)), mp.mpf(size)*(1+mp.mpf(w))
    gl, rigor = nbinom_score_hp(counts, n, S, left, prob)
    gr, _ = nbinom_score_hp(counts, n, S, right, prob)
    mean = Kahan()
    for v in x: mean.add(float(v)/n)
    dl, el = nbinom_core_score(counts, float(n), mean.value, float(left), prob)
    dr, er = nbinom_core_score(counts, float(n), mean.value, float(right), prob)
    vl, vr = sign_verdict(gl, +1), sign_verdict(gr, -1)
    verdict = 'fails' if 'fails' in (vl, vr) else 'holds' if vl == vr == 'holds' else 'open'
    offset = ''
    if verdict == 'holds':   # numerical: the root by bisection inside the signed bracket
        a, b = left, right
        for _ in range(200):
            m = (a+b)/2
            if not a < m < b: break
            gm, _ = nbinom_score_hp(counts, n, S, m, prob)
            if mp.mpf(gm.a) > 0: a = m
            elif mp.mpf(gm.b) < 0: b = m
            else: break
            if b-a < mp.mpf(10)**(-30)*b: break
        offset = '; hp root r*: (size - r*)/r* = %s' % mp.nstr((size-(a+b)/2)/((a+b)/2), 3)
    out.append(comparison('root', 'g(size*(1-5e-7)) > 0 > g(size*(1+5e-7))', verdict,
                          'left %s, right %s' % (show(gl), show(gr)), double='left %r, right %r' % (dl, dr),
                          margin='left %.3g, right %.3g' % (el, er), rigor=rigor,
                          note='core padded comparison at these points: left %s, right %s%s' %
                               ('signs' if dl > el else 'unsigned', 'signs' if dr < -er else 'unsigned', offset)))
    return out, None

# ---------------------------------------------------------------- lomax

class LomaxProfile:
    """Per-observation profile gap h(z) over the best exponential (lomax README, Global profile certificate)."""
    def __init__(self, x):
        counts = collections.Counter(x)
        self.n = len(x)
        total = sum(Fraction(v) for v in x)
        self.xbar = iv.mpf(total.numerator)/iv.mpf(total.denominator)/self.n
        self.ys = [(iv.mpf(v)/self.xbar, iv.mpf(c)/self.n) for v, c in sorted(counts.items())]
        self.M2 = sum((w*y*y for y, w in self.ys), iv.mpf(0))
        self.L = -sum((w*iv.log(y) for y, w in self.ys), iv.mpf(0))
        self.cache = {}
    def at(self, z):
        """(h, A, B, C) enclosures at an exact point z (mpf), cached."""
        key = mp.mpf(z)
        if key in self.cache: return self.cache[key]
        q = iv.exp(iv.mpf(key)); A = B = C = iv.mpf(0)
        for y, w in self.ys:
            r = q*y; p = r/(1+r)
            A += w*iv.log1p(r); B += w*p; C += w*p*(1-p)
        h = iv.mpf(key)-iv.log(A)-A
        self.cache[key] = (h, A, B, C)
        return self.cache[key]
    def score(self, z):
        h, A, B, C = self.at(z)
        return 1-B/A-B
    def curvature_upper(self, a, b):
        """Upper bound of h'' on [a, b]: A and B increase; p(1-p) is unimodal in z."""
        _, Aa, Ba, Ca = self.at(a); _, Ab, Bb, Cb = self.at(b)
        qa, qb = iv.exp(iv.mpf(a)), iv.exp(iv.mpf(b))
        low = iv.mpf(0)
        for y, w in self.ys:
            pa, pb = qa*y/(1+qa*y), qb*y/(1+qb*y)
            va, vb = pa*(1-pa), pb*(1-pb)
            low += w*iv.mpf(min(mp.mpf(va.a), mp.mpf(vb.a)))
        return Bb*Bb/(Aa*Aa)-low/Ab-low
    def curvature_floor(self, b):
        """c >= -inf_z h'' on z <= b: 5/4, or the small-q bound t/(1-t)+q with t = q*M2/2 (dossier)."""
        q = iv.exp(iv.mpf(b)); t = q*self.M2/2
        if mp.mpf(t.b) < 1:
            v = t/(1-t)+q
            return min(mp.mpf('1.25'), mp.mpf(v.b))
        return mp.mpf('1.25')
    def domain(self):
        """The certificate's compact interval and tail bounds: left h <= -log(1-qM2/2), right h < 0."""
        M2 = mp.mpf(self.M2.b)
        lo = mp.log(mp.mpf('1e-10'))-mp.log(max(mp.mpf(1), M2))
        L = mp.mpf(self.L.b)
        hi = L+mp.exp(L)+mp.mpf('1e-30')*(1+abs(L))   # strictly beyond L + e^L
        q = iv.exp(iv.mpf(lo))
        left_tail = -iv.log(1-q*self.M2/2)
        return lo, hi, left_tail

def lomax_bnb(p, lo, hi, target, exclude=None, budget=4000):
    """Interval branch and bound: is h(z) < target for every z in [lo, hi] outside `exclude`?

    Returns (verdict, info). An interval [a, b] is closed when the chord bound
    max(h(a), h(b)) + c (b-a)^2/8 lies below target, or when the score is
    signed throughout (A, B increase, so 1 - B_b/A_a - B_b <= h' <= 1 -
    B_a/A_b - B_a) and the endpoint that carries the maximum lies below target.
    An evaluated point with h > target is a counterexample."""
    parts = [(lo, hi)]
    if exclude is not None:
        el, er = exclude
        parts = [(a, b) for a, b in ((lo, min(el, hi)), (max(er, lo), hi)) if a < b]
    best = (mp.mpf('-inf'), None); evals = 0
    heap = []
    def push(a, b):
        nonlocal best, evals
        ha, Aa, Ba, _ = p.at(a); hb, Ab, Bb, _ = p.at(b); evals += 2
        for z, hz in ((a, ha), (b, hb)):
            if mp.mpf(hz.a) > best[0]: best = (mp.mpf(hz.a), z)
            if mp.mpf(hz.a) > target: return ('counter', z, hz)
        low = 1-Bb/Aa-Bb; high = 1-Ba/Ab-Ba
        if mp.mpf(low.a) > 0 and mp.mpf(hb.b) < target: return None
        if mp.mpf(high.b) < 0 and mp.mpf(ha.b) < target: return None
        c = p.curvature_floor(b)
        upper = max(mp.mpf(ha.b), mp.mpf(hb.b))+c*(b-a)**2/8
        if upper < target: return None
        heapq.heappush(heap, (-upper, a, b))
        return None
    for a, b in parts:
        hit = push(a, b)
        if hit: return 'fails', dict(point=hit[1], h=hit[2], evaluations=evals, best=best)
    while heap:
        if evals > budget:
            return 'open', dict(reason='budget of %d evaluations' % budget, upper=-heap[0][0], best=best, evaluations=evals)
        upper, a, b = heapq.heappop(heap)
        m = (a+b)/2
        if not (a < m < b): return 'open', dict(reason='interval at working precision', upper=-upper, best=best, evaluations=evals)
        for sub in ((a, m), (m, b)):
            hit = push(*sub)
            if hit: return 'fails', dict(point=hit[1], h=hit[2], evaluations=evals, best=best)
    return 'holds', dict(evaluations=evals, best=best)

def lomax_core_data(x):
    """Mirror of the certificate's normalization and arithmetic allowance eta (double mode)."""
    n = float(len(x)); largest = max(x)
    mean = Kahan()
    for v in x: mean.add((v/largest)/n)
    logmean = math.log(largest)+math.log(mean.value)
    eta = 256*EPS*(1+abs(math.log(largest))+abs(math.log(mean.value)))*(1+n*EPS)
    values = []; maxlog = 0.0
    xs = sorted(x); i = 0
    while i < len(xs):
        j = i+1
        while j < len(xs) and xs[j] == xs[i]: j += 1
        y = (xs[i]/largest)/mean.value; ly = math.log(y); maxlog = max(maxlog, abs(ly))
        values.append((y, ly, (j-i)/n)); i = j
    eta += 256*EPS*maxlog*(1+n*EPS)
    return dict(values=values, logmean=logmean, eta=eta)

def lomax_core_evaluate(d, z):
    """Mirror of lomax_cert_detail::evaluate for z >= -12: (h, score, error)."""
    a, b = Kahan(), Kahan()
    for y, ly, w in d['values']:
        t = z+ly; e = math.exp(-abs(t)); p = 1/(1+e) if t >= 0 else e/(1+e)
        a.add(w*(max(t, 0.)+math.log1p(e))); b.add(w*p)
    h = z-math.log(a.value)-a.value
    return h, 1-b.value/a.value-b.value, d['eta']*(1+abs(z)+abs(math.log(a.value))+a.value)

def lomax_core_held_score(x, kappa, t):
    """Mirror of the held-kappa log-rate score in lomax/mle.hpp (double-mode sum): (F, error)."""
    n = float(len(x)); logs = [math.log(v) for v in x]
    largest_log = max([0.0]+[abs(l) for l in logs]); logfactor = math.log1p(kappa)
    high = 0; small = []; maximum = -math.inf
    for l in logs:
        z = t+l
        if z >= 0: high += 1
        small.append(logfactor-abs(z)-math.log1p(math.exp(-abs(z)))); maximum = max(maximum, small[-1])
    if high > 0 and kappa > 4*n/high: return -1.0, 0.0
    constant = math.fma(-kappa, high, n-high)
    if constant != 0: maximum = max(maximum, math.log(abs(constant)))
    s = 0.0 if constant == 0 else math.copysign(math.exp(math.log(abs(constant))-maximum), constant)
    c = 0.0; absolute = abs(s)
    for l, sm in zip(logs, small):
        term = math.copysign(math.exp(sm-maximum), 1. if t+l >= 0 else -1.)
        y = term-c; u = s+y; c = (u-s)-y; s = u; absolute += abs(term)
    return s, 8*EPS*(1+abs(t)+largest_log+logfactor+abs(maximum))*absolute

def lomax_held_score_hp(x, kappa, t):
    """F(t)/1 = n - (kappa+1) sum_i logistic(t + log x_i), interval enclosure at exact t."""
    t = iv.mpf(t); total = iv.mpf(0)
    for v in x:
        total += 1/(1+iv.exp(-(t+iv.log(iv.mpf(v)))))
    return len(x)-(1+iv.mpf(kappa))*total

def replay_lomax(r, data, budget):
    out = []
    mask = r['mask']; res = r['result']; kind = (res.get('outcome') or {}).get('kind')
    x = [float(v) for v in data]
    if mask == 3: return out, 'evaluation only'
    if mask == 1: return out, 'closed form (lambda held)'
    if kind not in ('estimate', 'limit'): return out, None
    if mask == 2:
        lam = number(res['estimates']['lambda']); kappa = r['held']['kappa']
        t = mp.log(mp.mpf(lam)); w = mp.mpf('0.75e-10')
        fl = lomax_held_score_hp(x, kappa, t-w); fr = lomax_held_score_hp(x, kappa, t+w)
        vl, vr = sign_verdict(fl, +1), sign_verdict(fr, -1)
        dl, el = lomax_core_held_score(x, kappa, float(t-w)); dr, er = lomax_core_held_score(x, kappa, float(t+w))
        verdict = 'fails' if 'fails' in (vl, vr) else 'holds' if vl == vr == 'holds' else 'open'
        out.append(comparison('root', 'F(log(lambda)-0.75e-10) > 0 > F(log(lambda)+0.75e-10)', verdict,
                              'left %s, right %s' % (show(fl), show(fr)), double='left %r, right %r' % (dl, dr),
                              margin='left %.3g, right %.3g' % (el, er),
                              note='core padded comparison at these points: left %s, right %s' %
                                   ('signs' if dl > el else 'unsigned', 'signs' if dr < -er else 'unsigned')))
        return out, None
    # all free
    if len(set(x)) == 1 and kind == 'limit':
        out.append(comparison('face', 'constant sample: exact exponential outcome', 'holds', 'h(z) < 0 for all z (exact)',
                              rigor='exact'))
        return out, None
    p = LomaxProfile(x)
    lo, hi, left_tail = p.domain()
    margin = mp.mpf('1e-8')
    core = lomax_core_data(x)
    n = len(x)
    if kind == 'limit':
        tail_ok = mp.mpf(left_tail.b) < margin
        verdict, info = lomax_bnb(p, lo, hi, margin, budget=budget)
        if verdict == 'holds' and not tail_ok: verdict = 'open'
        best, where = info['best']
        hp = 'sup h < 1e-8 on [%s, %s]; left tail <= %s; best h found %s at z=%s' % (
            mp.nstr(lo, 8), mp.nstr(hi, 8), mp.nstr(mp.mpf(left_tail.b), 3), mp.nstr(best, 6),
            mp.nstr(where, 8) if where is not None else '-')
        if verdict == 'fails': hp = 'h(%s) = %s > 1e-8' % (mp.nstr(info['point'], 12), show(info['h']))
        if verdict == 'open': hp = '%s; open: %s' % (hp, info.get('reason', 'left tail'))
        label, text = lomax_face_class(p, lo, hi, budget)
        out.append(comparison('face', 'exponential limit: h(z) < 1e-8 for every z (no gain above the margin)',
                              verdict, hp, double=None, margin=1e-8,
                              note='%d evaluations; supremum: %s' % (info['evaluations'], text), info=label))
        return out, None
    lam = number(res['estimates']['lambda']); kappa = number(res['estimates']['kappa'])
    zhat = iv.log(iv.mpf(lam)*p.xbar); zc = mp.mpf(zhat.a)/2+mp.mpf(zhat.b)/2
    hz, A, B, C = p.at(zc)
    # gain: the comparison that selects the finite fit over the exponential limit
    gain_double = number(res['logLik'])/n+core['logmean']+1
    hcore, scorecore, errcore = lomax_core_evaluate(core, math.log(lam)+core['logmean']) if math.log(lam)+core['logmean'] >= -12 else (None, None, None)
    try: star = lomax_local_max(p, zc)
    except (ValueError, ZeroDivisionError): star = None
    offset = '; hp root: z_hat - z* = %s' % mp.nstr(zc-star[0], 3) if star is not None else ''
    out.append(comparison('gain', 'h(z_hat) > 1e-8', above(hz, margin), show(hz), double=gain_double,
                          margin='1e-8 + core error %s' % (('%.3g' % errcore) if errcore is not None else 'n/a'),
                          note='double gain from the reported logLik%s' % offset))
    W = mp.mpf('8e-7')
    zl, zr = zc-W, zc+W
    sl, sr = p.score(zl), p.score(zr)
    vl, vr = sign_verdict(sl, +1), sign_verdict(sr, -1)
    dl = lomax_core_evaluate(core, float(zl)) if float(zl) >= -12 else None
    dr = lomax_core_evaluate(core, float(zr)) if float(zr) >= -12 else None
    root = 'fails' if 'fails' in (vl, vr) else 'holds' if vl == vr == 'holds' else 'open'
    out.append(comparison('root', "h'(z_hat-8e-7) > 0 > h'(z_hat+8e-7)", root, 'left %s, right %s' % (show(sl), show(sr)),
                          double='left %r, right %r' % (dl and dl[1], dr and dr[1]),
                          margin='4 eta = %.3g' % (4*core['eta']) if dl else None))
    cu = p.curvature_upper(zl, zr)
    concave = sign_verdict(cu, -1)
    out.append(comparison('concave', "h'' < 0 on the enclosure (termwise bound)", concave, show(cu),
                          margin='4 eta = %.3g' % (4*core['eta'])))
    if root == 'holds' and concave == 'holds':
        target = mp.mpf(hz.a)
        verdict, info = lomax_bnb(p, lo, hi, target, exclude=(zl, zr), budget=budget)
        tail_ok = mp.mpf(left_tail.b) < target and target > 0
        if verdict == 'holds' and not tail_ok: verdict = 'open'
        hp = 'h < h(z_hat) = %s outside the enclosure on [%s, %s]; left tail <= %s; best other h %s' % (
            mp.nstr(target, 12), mp.nstr(lo, 8), mp.nstr(hi, 8), mp.nstr(mp.mpf(left_tail.b), 3), mp.nstr(info['best'][0], 12))
        if verdict == 'fails': hp = 'h(%s) = %s > h(z_hat) = %s' % (mp.nstr(info['point'], 12), show(info['h']), mp.nstr(target, 12))
        if verdict == 'open': hp += '; open: %s' % info.get('reason', 'left tail')
        out.append(comparison('global', 'z_hat carries the global maximum of the profile', verdict, hp,
                              note='%d evaluations' % info['evaluations']))
    else:
        out.append(comparison('global', 'z_hat carries the global maximum of the profile', 'open',
                              'not run: the enclosure is not established'))
    return out, None

def lomax_face_class(p, lo, hi, budget):
    """Information for a margin-decided exponential limit: is the supremum the exponential itself?

    Proved 'none' (no finite maximizer, supremum unattained) when h < 0 on
    [lo, hi] by branch and bound and on both tails: on z <= lo, A >= q -
    q^2 M2/2 gives h <= f(q) = -log(1-qM2/2) - q + q^2 M2/2, with f(0) = 0
    and f convex, so f < 0 on (0, q_lo] when f'(q_lo) < 0 (which needs
    M2 < 2); on z >= hi, h < 0 as for the margin check. 'finite' when an
    interval evaluation shows h > 0 somewhere: a finite maximizer exists and
    its gain (numerical local maximum) lies below the margin."""
    q = iv.exp(iv.mpf(lo)); M2 = p.M2
    fprime = (M2/2)/(1-q*M2/2)-1+q*M2
    verdict, info = lomax_bnb(p, lo, hi, mp.mpf(0), budget=budget)
    if verdict == 'holds' and mp.mpf(fprime.b) < 0:
        return 'none', 'proved h < 0 for every finite z (M2 = %s < 2): no finite maximizer; the exponential supremum is not attained' % mp.nstr(mp.mpf(M2.a), 6)
    if verdict == 'fails':
        found = mp.mpf(info['h'].a)
        try: sup = lomax_local_max(p, info['point'])
        except (ValueError, ZeroDivisionError): sup = None
        gain = sup[1] if sup is not None and sup[1] >= found else found
        return 'finite', 'h > 0 at z = %s: a finite maximizer exists, gain %s per observation (%s in total) below the margin' % (
            mp.nstr(info['point'], 8), mp.nstr(gain, 6), mp.nstr(gain*p.n, 6))
    return 'undetermined', 'sign of the supremum undetermined (%s; M2 = %s)' % (info.get('reason', 'left tail'), mp.nstr(mp.mpf(M2.a), 6))

def lomax_local_max(p, z):
    """Newton on h' from z in ordinary mpmath (numerical): (z*, h(z*)) or None."""
    mp_ys = [(mp.mpf(y.a)/2+mp.mpf(y.b)/2, mp.mpf(w.a)/2+mp.mpf(w.b)/2) for y, w in p.ys]
    def parts(z):
        q = mp.exp(z); A = B = C = mp.mpf(0)
        for y, w in mp_ys:
            r = q*y; pp = r/(1+r); A += w*mp.log1p(r); B += w*pp; C += w*pp*(1-pp)
        return A, B, C
    z = mp.mpf(z)
    for _ in range(100):
        A, B, C = parts(z)
        g = 1-B/A-B; H = B*B/(A*A)-C/A-C
        if not H < 0: return None
        step = g/H; z -= step
        if abs(step) < mp.mpf(10)**(-30)*(1+abs(z)): break
    A, _, _ = parts(z)
    return z, z-mp.log(A)-A

# ---------------------------------------------------------------- driver

REPLAYS = {'nbinom': replay_nbinom, 'lomax': replay_lomax}
import cert_replay_roots   # mechanisms B and C: scalar roots and represented likelihood (cert_replay_roots.py)
REPLAYS.update(cert_replay_roots.REPLAYS)

import cert_replay_faces   # mechanism D: exact face inequalities (cert_replay_faces.py)

def combined(*replays):
    """A family replayed by several modules: concatenate their comparisons."""
    def replay(r, data, budget):
        comps, whys = [], []
        for one in replays:
            c, why = one(r, data, budget)
            comps += c or []
            if why: whys.append(why)
        return comps, '; '.join(whys) or None
    return replay

for family, replay in cert_replay_faces.REPLAYS.items():
    REPLAYS[family] = combined(REPLAYS[family], replay) if family in REPLAYS else replay

import cert_replay_integer   # mechanism G: exhaustive integer searches (cert_replay_integer.py)
for family, replay in cert_replay_integer.REPLAYS.items():
    REPLAYS[family] = combined(REPLAYS[family], replay) if family in REPLAYS else replay

def rebuild(family, archive):
    routes = {route['id']: ident for ident, route in fs.ROUTES.items()}   # modifier routes are recorded under their route id
    ident = routes.get(family, family)
    fs.ROUTE_MODE = family in routes
    try:
        meta, _ = fs.read(ident)
        names = [p['name'] for p in meta['params']]
        return {(c['source'], c['case']): (c, hashlib.sha256(fs.line(c, names).encode()).hexdigest()[:16])
                for c in fs.cases(ident, archive)}
    finally:
        fs.ROUTE_MODE = False

def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('native'); ap.add_argument('double', nargs='?')
    ap.add_argument('--family', default=','.join(sorted(REPLAYS)), help='comma-separated families (default: all replayed)')
    ap.add_argument('--records', help='write one JSON line per replayed case')
    ap.add_argument('--dps', type=int, default=50, help='interval working precision in digits')
    ap.add_argument('--budget', type=int, default=4000, help='profile evaluations per branch and bound')
    ap.add_argument('--case', metavar='REGEX', help='replay only cases whose source/case matches')
    add_arguments(ap)
    args = ap.parse_args()
    iv.dps = args.dps; mp.mp.dps = args.dps
    families = [f for f in args.family.split(',') if f]
    for f in families:
        if f not in REPLAYS: raise SystemExit('no replay for %s (have: %s)' % (f, ', '.join(sorted(REPLAYS))))
    archive = {}
    for c in json.loads(gzip.decompress(fs.ARCHIVE.read_bytes())): archive.setdefault(c['family'], []).append(c)
    snapshots = {mode_of(p): load(p) for p in filter(None, (args.native, args.double))}
    failures = {}; judged = set(); opened = []; tally = collections.Counter(); skipped = collections.Counter()
    information = collections.Counter()
    records = open(args.records, 'w') if args.records else None
    started = time.process_time()
    for family in families:
        inputs = rebuild(family, archive)
        for mode, rows in snapshots.items():
            for (fam, source, case), r in sorted(rows.items()):
                if fam != family: continue
                if args.case and not re.search(args.case, source+'/'+case): continue
                entry = inputs.get((source, case))
                if entry is None: raise SystemExit('%s %s %s: not among fit-snapshot.py cases' % (family, source, case))
                c, digest = entry
                if r['input_sha256'] != digest: raise SystemExit('%s %s %s: snapshot input differs' % (family, source, case))
                t0 = time.process_time()
                comps, why = REPLAYS[family](r, c['data'], args.budget)
                if not comps:
                    skipped['%s: %s' % (family, why or 'no certified outcome (%s)' % r['result']['kind'])] += 1
                    continue
                judged.add((mode, family, source, case))
                for comp in comps:
                    tally[(family, comp['name'], comp['verdict'])] += 1
                    if comp.get('info'): information[(family, comp['name'], comp['info'])] += 1
                    key = '|'.join(('%s:%s' % (CHECK, comp['name']), mode, family, source, case))
                    text = '%s; hp %s; double %s; margin %s%s' % (comp['claim'], comp['hp'], comp['double'], comp['margin'],
                                                                '; '+comp['note'] if comp['note'] else '')
                    if comp['verdict'] == 'fails': failures[key] = text
                    elif comp['verdict'] == 'open': opened.append((key, text))
                if records:
                    records.write(json.dumps(dict(mode=mode, family=family, source=source, case=case, mask=r['mask'],
                                                  kind=r['result']['kind'], seconds=round(time.process_time()-t0, 3),
                                                  comparisons=comps), default=str)+'\n')
    if records: records.close()
    print('cert-replay: %.0f CPU s, %d digits' % (time.process_time()-started, args.dps))
    for (family, name, verdict), count in sorted(tally.items()):
        print('  %-7s %-11s %-6s %d' % (family, name, verdict, count))
    for (family, name, label), count in sorted(information.items()):
        print('  %-7s %-11s supremum %s: %d' % (family, name, label, count))
    if skipped:
        print('not replayed (no certificate comparison):')
        for k, v in sorted(skipped.items()): print('  %s: %d' % (k, v))
    if opened:
        print('open (not judged):')
        for key, text in opened: print('  OPEN  %s: %s' % (key, text))
    print('%d cases judged' % len(judged))
    sys.exit(ratchet(failures, judged, lambda check: check.startswith(CHECK+':'), args, 'cert-replay'))

if __name__ == '__main__': main()
