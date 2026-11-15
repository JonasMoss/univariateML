"""Printable examples for the report: binom exact tie, gompertz near tie, pareto2/lomax gain, in exact arithmetic."""
import json, math, os, sys
from fractions import Fraction
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import binom_boundary as bb, gompertz_boundary as gb

x = [68, 56, 51, 51, 44, 43, 49, 59, 53]
n = len(x); S = sum(x); Q = sum(v*v for v in x)
print('binom n=%d x=%s' % (n, x))
print('  S=%d Q=%d  delta = nS + S^2 - nQ = %d   (mean = %s, population variance = %s)' % (S, Q, n*S+S*S-n*Q, Fraction(S, n), Fraction(n*Q-S*S, n*n)))
for fma in (False, True):
    p, r = bb.mirror(x, fma); print('  binary64 %s: x_bar/var_pop = %.17g -> %s' % ('with fma' if fma else 'plain', r, 'Poisson limit' if p else 'finite (continues to the size search)'))
rows = json.load(open('gompertz_boundary.json'))
w = [r for r in rows if r['x'] and r['n'] == 3 and all(r[m] != r['truth'] for m in ('ldplain', 'dplain', 'ldfma', 'dfma'))]
r = sorted(w, key=lambda r: abs(r['gap']))[0]
x = r['x']; d, S = gb.exact_delta(x)
print('\ngompertz n=3 x=%r' % x)
print('  exact: n Q - 2 S^2 = %s  (relative %.3e) -> %s' % (float(d), float(d/(2*S*S)), 'exponential limit' if d >= 0 else 'finite MLE'))
print('  long double mirror: %s, double mirror: %s' % ('limit' if gb.mirror(x, True, False) else 'finite', 'limit' if gb.mirror(x, False, False) else 'finite'))
