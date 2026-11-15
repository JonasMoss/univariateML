"""Error of the GED base-tail evaluator used by the psged fast path (TASK-220).

The fast path (snorm_detail::probability_with_moment) evaluates
upper(v)=pged(v,0,1,nu,upper,log) at the rounded double radius v and
reserves 2e-12 for that evaluator. The GED log tail has log-radius
sensitivity v*h(v), about nu*(v/s)^nu+nu-1, so any error in the
evaluator's internal log radius log(v)-log(s) is amplified by that factor.
This script measures |upper(v)-log S(v)| at exact double radii and reports
it both absolutely and in units of eps*v*h(v) (the radius-equivalent error).

    python3 evaluator.py --binary EVAL [--op g|u] --out evaluator.json
"""
import argparse
import gzip
import json
import math
import subprocess
import mpmath as mp

def dump(value, path):
    """Write JSON, gzip-compressed when the name ends in .gz."""
    with (gzip.open(path, 'wt') if str(path).endswith('.gz') else open(path, 'w')) as f:
        json.dump(value, f, indent=1)


SHAPES = [3.0, 3.4208324500711944, 5.0, 10.0, 30.0, 60.0, 100.0, 1e3, 1e4, 1e5, 999999.0]
TARGETS = [-0.01, -0.1, -1.0, -10.0, -50.0, -100.0, -200.0, -300.0, -400.0, -500.0, -600.0,
           -650.0, -690.0, -700.0, -705.0, -708.0, -708.39]
EPS = 2.0**-52


def reference(nu, v):
    nu, v = mp.mpf(nu), mp.mpf(v)
    a = 1/nu
    s = mp.sqrt(mp.gamma(a)/mp.gamma(3*a))
    y = (v/s)**nu
    logS = mp.log(mp.gammainc(a, y, mp.inf, regularized=True)/2)
    logf = mp.log(nu)-mp.log(2*s)-mp.loggamma(a)-y
    return logS, mp.exp(logf-logS)*v


def radius_for(nu, lp):
    """Double radius whose GED upper log tail is near lp (placement only)."""
    nu = mp.mpf(nu)
    a = 1/nu
    s = mp.sqrt(mp.gamma(a)/mp.gamma(3*a))
    f = lambda t: mp.log(mp.gammainc(a, mp.exp(t), mp.inf, regularized=True)/2)-lp
    t = mp.findroot(f, (mp.log(max(mp.mpf('1e-6'), -lp)), mp.log(-lp+30)), solver='anderson')
    return float(s*mp.exp(t/nu))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--binary', required=True)
    parser.add_argument('--out', required=True)
    parser.add_argument('--op', default='g', choices=('g', 'u'),
                        help='g: pged (baseline evaluator); u: two-word log(v/s) evaluator (current tree)')
    args = parser.parse_args()
    mp.mp.dps = 60
    cases = []
    for nu in SHAPES:
        for lp in TARGETS:
            try:
                v = radius_for(nu, mp.mpf(lp))
            except (ValueError, ZeroDivisionError):
                continue
            for k in range(-2, 3):
                cases.append((nu, v*(1+k*EPS*7)))
    lines = [f'{args.op} ged 0 1 {nu!r} 0 {v!r}' for nu, v in cases]
    out = subprocess.run([args.binary], input='\n'.join(lines)+'\n', capture_output=True, text=True, check=True)
    rows = []
    for (nu, v), line in zip(cases, out.stdout.split()):
        got = float(line)
        logS, sensitivity = reference(nu, v)
        error = abs(mp.mpf(got)-logS) if math.isfinite(got) else mp.inf
        rows.append(dict(nu=nu, v=v, logS=float(logS), got=got, error=float(error),
                         sensitivity=float(sensitivity), radius_eps=float(error/(EPS*sensitivity))))
    worst = {}
    for r in rows:
        w = worst.setdefault(r['nu'], dict(nu=r['nu'], max_error=0.0, max_radius_eps=0.0, at_logS=None))
        if r['error'] > w['max_error']:
            w.update(max_error=r['error'], at_logS=r['logS'])
        w['max_radius_eps'] = max(w['max_radius_eps'], r['radius_eps'])
    dump(dict(rows=rows, worst=list(worst.values())), args.out)
    print(f'{"nu":>10} {"max |err|":>10} {"at log S":>9} {"max err/(eps v h)":>18}')
    for w in worst.values():
        print(f'{w["nu"]:10.6g} {w["max_error"]:10.3g} {w["at_logS"]:9.3g} {w["max_radius_eps"]:18.3g}')


if __name__ == '__main__':
    main()
