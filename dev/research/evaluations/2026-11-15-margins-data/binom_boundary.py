"""binom Poisson-limit decision at the exact boundary.

Criterion (README existence; mle.hpp `x_bar / var_pop <= 1.0`):   Poisson limit  iff  delta <= 0,
    delta = n S + S^2 - n Q = n^2 (mean - population variance),  S = sum x, Q = sum x^2.

Construction of integer samples with delta exactly in {-2..2}.  delta(x + t) = delta(x) + n^2 t  (shift),
and delta = n S + S^2 - n Q = S^2 (mod n).  Take S with S^2 = target (mod n), fix Q mod n with
sum-preserving pair moves (y_a += k, y_b -= k changes Q by 2k(y_a - y_b + k)), then shift by
t = (target - delta(y)) / n^2.  The final delta is recomputed in exact integers.

Mirrors of the C++ evaluation (binom/mle.hpp lines 71-90), all in binary64:
  sum_x += v;  x_bar = sum_x / dn;  var += d*d, d = v - x_bar;  var /= dn;  decide x_bar / var <= 1.0
  'plain'  every operation rounded;   'fma'  var = fma(d, d, var) (a contracting compiler, e.g. aarch64 GCC)
"""
import math, random, sys, json
import numpy as np

def mirror(x, fma):
    sx = 0.0
    for v in x: sx += float(v)
    n = float(len(x)); xbar = sx/n; var = 0.0
    for v in x:
        d = float(v)-xbar
        var = math.fma(d, d, var) if fma else var+d*d
    var /= n
    ratio = xbar/var if var else math.inf
    return ratio <= 1.0, ratio                      # True: Poisson limit

def residues(n, target):
    return [r for r in range(n) if (r*r-target) % n == 0]

def build(n, R, target, rng, tries=200):
    res = [r for r in residues(n, target) if r % n != 0]   # non-integer mean
    if not res: return None
    for _ in range(tries):
        y = [rng.randrange(R) for _ in range(n)]
        r = rng.choice(res)
        S0 = sum(y); S = S0-(S0-r) % n                 # S = r (mod n), close to S0
        y[-1] += S-S0
        if not (0 <= y[-1] < max(R, 2*R)): continue
        S = sum(y); assert (S*S-target) % n == 0
        need = (S+(S*S-target)//n) % n                 # Q must be = need (mod n)
        Q = sum(v*v for v in y)
        ks = np.arange(n, dtype=object)
        for _ in range(200):
            if Q % n == need: break
            a, b = rng.sample(range(n), 2)
            D = y[a]-y[b]
            good = [k for k in range(1, n) if (Q+2*k*(D+k)) % n == need]
            if good:
                k = rng.choice(good); y[a] += k; y[b] -= k; Q += 2*k*(D+k); break
        S = sum(y); Q = sum(v*v for v in y)
        if Q % n != need: continue
        d = n*S+S*S-n*Q
        if (target-d) % (n*n): continue
        t = (target-d)//(n*n)
        x = [v+t for v in y]
        if min(x) < 0 or max(x) >= 2**31: continue
        S = sum(x); Q = sum(v*v for v in x)
        assert n*S+S*S-n*Q == target
        return x
    return None

def run():
    rng = random.Random(20261115)
    rows = []
    for R in (30, 300, 3000, 30000, 150000):
        for n in (9, 13, 25, 36, 100, 255, 1000, 2000, 4000):
            for target in (-2, -1, 0, 1, 2):
                for rep in range(12 if n <= 255 else 3):
                    if not residues(n, target) or all(r % n == 0 for r in residues(n, target)): break
                    x = build(n, R, target, rng)
                    if x is None: continue
                    S = sum(x); Q = sum(v*v for v in x)
                    truth = target <= 0
                    pl, rp = mirror(x, False); fm, rf = mirror(x, True)
                    rows.append(dict(R=R, n=n, target=target, mean=S/n, var=(n*Q-S*S)/n**2, gap=target/(n*Q-S*S),
                                     truth=truth, plain=pl, fma=fm, ratio_plain=rp, ratio_fma=rf, x=x if n <= 255 else None))
    return rows

if __name__ == '__main__':
    rows = run()
    json.dump(rows, open(sys.argv[1] if len(sys.argv) > 1 else 'binom_boundary.json', 'w'))
    # summary by magnitude (mean ~ var) and by delta
    print('samples constructed: %d' % len(rows))
    print('%8s %6s | %5s %7s | wrong(plain) wrong(fma) | |gap| range' % ('mean~', 'n', 'delta', 'count'))
    from collections import defaultdict
    g = defaultdict(list)
    for r in rows: g[(round(math.log10(r['mean'])), r['n'], r['target'])].append(r)
    bymag = defaultdict(lambda: [0, 0, 0])
    for r in rows:
        k = round(math.log10(r['mean'])); b = bymag[k]; b[0] += 1; b[1] += r['plain'] != r['truth']; b[2] += r['fma'] != r['truth']
    print('\nby magnitude of mean (log10):   samples   wrong(plain)   wrong(fma)')
    for k in sorted(bymag): print('  1e%-3d %14d %12d %12d' % (k, *bymag[k]))
    print('\nby n (all deltas):   samples   wrong(plain)   wrong(fma)   median |gap|')
    byn = defaultdict(list)
    for r in rows: byn[r['n']].append(r)
    for n in sorted(byn):
        v = byn[n]; gaps = sorted(abs(r['gap']) for r in v)
        print('  %5d %9d %12d %12d   %.2e' % (n, len(v), sum(r['plain'] != r['truth'] for r in v), sum(r['fma'] != r['truth'] for r in v), gaps[len(gaps)//2]))
    print('\nwrong decisions (exact boundary side vs binary64), first 12 by smallest n:')
    bad = sorted([r for r in rows if r['plain'] != r['truth'] or r['fma'] != r['truth']], key=lambda r: (r['n'], r['R']))
    for r in bad[:12]:
        print('  n=%d R=%d delta=%+d mean=%.6g var=%.6g gap=%+.2e exact=%s plain=%s fma=%s' % (
            r['n'], r['R'], r['target'], r['mean'], r['var'], r['gap'], 'poisson' if r['truth'] else 'finite',
            'poisson' if r['plain'] else 'finite', 'poisson' if r['fma'] else 'finite'))
