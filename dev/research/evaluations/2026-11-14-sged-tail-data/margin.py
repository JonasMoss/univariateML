"""Margin decomposition at the two refused truth rows (sged point 7: nu=30, xi=3).

    python3 margin.py --baseline EVAL_BASELINE --current EVAL_CURRENT --out margin.json

Prints the fast-path terms of snorm_detail::probability_with_moment as the
baseline computes them, the exact hazard over the radius-error interval, the
exact log tail at the rounded radius, and the errors of both builds' outputs.
"""
import argparse
import json
import subprocess
import mpmath as mp

ROWS = [(-1.8189346365433414, True), (2.548015429452742, False)]
NU, XI = 30.0, 3.0
EPS = 2.0**-52


def run(binary, lines):
    return subprocess.run([binary], input='\n'.join(lines)+'\n', capture_output=True, text=True,
                          check=True).stdout.splitlines()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--baseline', required=True)
    parser.add_argument('--current', required=True)
    parser.add_argument('--out', required=True)
    args = parser.parse_args()
    mp.mp.dps = 60
    nu = mp.mpf(NU)
    a = 1/nu
    s = mp.sqrt(mp.gamma(a)/mp.gamma(3*a))
    logS = lambda v: mp.log(mp.gammainc(a, (v/s)**nu, mp.inf, regularized=True)/2)
    hazard = lambda v: mp.exp(mp.log(nu)-mp.log(2*s)-mp.loggamma(a)-(v/s)**nu-logS(v))
    # Exact skew coordinate (README density): y = scale*(x-m)/sd + shift.
    m1 = mp.gamma(2/nu)/mp.sqrt(mp.gamma(1/nu)*mp.gamma(3/nu))
    shift = m1*(XI-1/mp.mpf(XI))
    scale = mp.sqrt((1-m1*m1)*(XI**2+1/mp.mpf(XI)**2)+2*m1*m1-1)
    out = []
    for x, lower in ROWS:
        v, centering, radius_error, hazard_bound, thin = map(float, run(args.baseline, [f'm sged 0 1 {NU!r} {XI!r} {x!r}'])[0].split())
        y = scale*mp.mpf(x)+shift
        # Radius in the unit-variance GED coordinate (truth.py convention).
        exact_radius = abs(y*XI) if y < 0 else abs(y/XI)
        e = mp.mpf(radius_error)
        h_lo, h_hi = hazard(mp.mpf(v)-e), hazard(mp.mpf(v)+e)
        weight = mp.log(2/(1+mp.mpf(XI)**2)) if lower else mp.log(2*mp.mpf(XI)**2/(1+mp.mpf(XI)**2))
        exact = weight+logS(exact_radius)
        at_rounded = weight+logS(mp.mpf(v))
        g_base = float(run(args.baseline, [f'g ged 0 1 {NU!r} 0 {v!r}'])[0])
        u_curr = float(run(args.current, [f'u ged 0 1 {NU!r} 0 {v!r}'])[0])
        base = [float(t) for t in run(args.baseline, [f'p sged 0 1 {NU!r} {XI!r} {x!r}'])[0].split()]
        curr = [float(t) for t in run(args.current, [f'p sged 0 1 {NU!r} {XI!r} {x!r}'])[0].split()]
        k = 0 if lower else 1
        margin = radius_error*hazard_bound+2e-12+32*EPS
        row = dict(x=x, lower=lower, radius=v, exact_radius=mp.nstr(exact_radius, 25),
                   radius_rounding=float(abs(mp.mpf(v)-exact_radius)), half_ulp_bound=v*EPS/2,
                   centering_term=centering, radius_error=radius_error, hazard_bound=hazard_bound,
                   exact_hazard_low=float(h_lo), exact_hazard_high=float(h_hi), margin=margin,
                   margin_radius_part=radius_error*hazard_bound,
                   exact_log_tail=mp.nstr(exact, 25),
                   rounding_effect=float(at_rounded-exact),
                   evaluator_error_baseline=float(mp.mpf(g_base)-logS(mp.mpf(v))),
                   evaluator_error_current=float(mp.mpf(u_curr)-logS(mp.mpf(v))),
                   baseline=dict(log=base[k], natural=base[2+k],
                                 log_error=float(mp.mpf(base[k])-exact)),
                   current=dict(log=curr[k], natural=curr[2+k],
                                log_error=float(mp.mpf(curr[k])-exact),
                                natural_relative_error=float((mp.mpf(curr[2+k])-mp.exp(exact))/mp.exp(exact))))
        out.append(row)
        print(json.dumps(row, indent=1))
    json.dump(out, open(args.out, 'w'), indent=1)


if __name__ == '__main__':
    main()
