"""Independent normal-cell scores, stored fit audit, and a source replay."""
import gzip
import json
import math
from collections import Counter
from pathlib import Path

import mpmath as mp


EVAL = Path(__file__).resolve().parents[1]
ROOT = EVAL.parents[2]
FAMILY = ROOT / "univariateML-cpp/include/cppunivariate/distributions/dnorm"
TOL = mp.mpf("1e-10")


def normal_interval(a, b, num):
    """Use the lower CDF or its reflected form to avoid subtracting from 1."""
    if a + b > 0:
        a, b = -b, -a
    return (num.erfc(-b / num.sqrt(2)) - num.erfc(-a / num.sqrt(2))) / 2


def exact_eval(hist, mu, sd):
    """Return total log likelihood and scores in location and log scale."""
    ll = dm = ds = mp.mpf(0)
    for x, count in hist.items():
        a = (x - mu) / sd
        b = (x + 1 - mu) / sd
        p = normal_interval(a, b, mp)
        left = mp.exp(-a*a/2) / mp.sqrt(2*mp.pi)
        right = mp.exp(-b*b/2) / mp.sqrt(2*mp.pi)
        ll += count * mp.log(p)
        dm += count * (left - right) / (sd * p)
        ds += count * (a*left - b*right) / p
    return ll, dm, ds


def root(hist, holds, start):
    offset = min(hist)
    hist = {x-offset: count for x, count in hist.items()}
    mu, sd = map(mp.mpf, start)
    mu -= offset
    if "mean" in holds and "sd" in holds:
        return mu + offset, sd
    if "mean" in holds:
        t = mp.findroot(lambda t: exact_eval(hist, mu, mp.exp(t))[2], mp.log(sd))
    elif "sd" in holds:
        mu = mp.findroot(lambda m: exact_eval(hist, m, sd)[1], mu)
        t = mp.log(sd)
    else:
        def score(m, t):
            return exact_eval(hist, m, mp.exp(t))[1:]
        mu, t = mp.findroot(score, (mu, mp.log(sd)))
    sd = mp.exp(t) if "sd" not in holds else sd
    values = exact_eval(hist, mu, sd)
    assert all(abs(values[i]) < mp.mpf("1e-55") for i, name in
               enumerate(("mean", "sd"), 1) if name not in holds)
    return mu + offset, sd


def errors(candidate, star, holds):
    return [float(abs(mp.mpf(y)-z)/unit) if name not in holds else None
            for name, y, z, unit in zip(("mean", "sd"), candidate, star,
                                       (max(abs(star[0]), star[1]), star[1]))]


def check_actual():
    golden = json.loads((FAMILY / "golden.json").read_text())
    hist = Counter(golden["data"])
    candidate = tuple(golden["estimates"][name] for name in ("mean", "sd"))
    star = root(hist, {}, candidate)
    err = errors(candidate, star, {})
    assert max(err) < TOL
    print("golden", err, "root", [mp.nstr(y, 24) for y in star])

    with gzip.open(EVAL / "2026-11-07-fit-audit-data/cases.json.gz", "rt") as stream:
        cases = {row["index"]: row for row in json.load(stream)}
    selections = {1098, 1099, 1100, 1116, 1117, 1118, 1119, 1124}
    for mode in ("native", "double"):
        found = set()
        path = EVAL / f"2026-11-15-fit-snapshot-data/{mode}.jsonl.gz"
        with gzip.open(path, "rt") as stream:
            for line in stream:
                row = json.loads(line)
                if row["family"] != "dnorm" or row["source"] != "archive":
                    continue
                case = int(row["case"])
                if case not in selections or row["result"]["kind"] != "ok":
                    continue
                found.add(case)
                sample = cases[case]["data"]
                holds = row["held"]
                candidate = tuple(float.fromhex(row["result"]["estimates"][name])
                                  for name in ("mean", "sd"))
                assert all(candidate[("mean", "sd").index(name)] == (float.fromhex(val) if isinstance(val, str) else val)
                           for name, val in holds.items())
                star = root(Counter(sample), holds, candidate)
                if case == 1099:
                    assert abs(star[0]+mp.mpf("0.5")) < mp.mpf("1e-90")
                    assert abs(star[1]-1/mp.sqrt(mp.log(3))) < mp.mpf("1e-90")
                err = errors(candidate, star, holds)
                ll = exact_eval(Counter(sample), *map(mp.mpf, candidate))[0]
                published_ll = mp.mpf(float.fromhex(row["result"]["logLik"]))
                ll_error = float(abs(ll-published_ll)/max(1, abs(ll)))
                assert ll_error < TOL
                print(mode, case, "holds", holds, "counts", sorted(Counter(sample).items()),
                      "errors", err, "logLik error", ll_error)
        assert found == selections


def working_eval(hist, center, A, B, held, sd, num):
    """Match the ordinary-interval gradient/Hessian and held-coordinate arithmetic."""
    npar = 1 if held is not None or sd is not None else 2
    g = [0] * npar
    H = [[0] * npar for _ in g]
    total = 0
    for x, count in sorted(hist.items()):
        lo = x - center
        hi = lo + 1
        a, b = lo*B-A, hi*B-A
        assert B*(1+abs((lo+0.5)*B-A)) >= 1e-5  # all replay cases use this branch
        if num is math:
            # Same log-tail subtraction as dnorm_interval, with libm erfc.
            low, high = (a, b) if a+b < 0 else (-b, -a)
            log_lo, log_hi = [math.log(math.erfc(-z/math.sqrt(2))/2)
                              for z in (low, high)]
            d = log_lo-log_hi
            lp = log_hi + (math.log1p(-math.exp(d)) if d < -math.log(2)
                           else math.log(-math.expm1(d)))
        else:
            lp = num.log(normal_interval(a, b, num))
        total -= count * lp
        c = num.log(2*num.pi)/2
        fa = num.exp(-0.5*a*a-c-lp)
        fb = num.exp(-0.5*b*b-c-lp)
        if held is not None:
            va, vb = [lo-held], [hi-held]
        elif sd is not None:
            va = vb = [-1]
        else:
            va, vb = [-1, lo], [-1, hi]
        s = [fb*vb[k]-fa*va[k] for k in range(npar)]
        for k in range(npar):
            g[k] -= count*s[k]
            for j in range(npar):
                H[k][j] += count*(s[k]*s[j]+b*fb*vb[k]*vb[j]-a*fa*va[k]*va[j])
    return total, g, H


def replay(hist, *, num, mean=None, sd=None, maxit=200):
    """Python float uses libm erfc in place of the C++ interval backend."""
    n = sum(hist.values())
    value = float if num is math else mp.mpf
    offset = min(hist)
    center = sum(value(w)*(x-offset)/n for x, w in sorted(hist.items()))
    center += value(offset) + 0.5
    variance = sum(value(w)*(x+0.5-center)*(x+0.5-center)/n
                   for x, w in sorted(hist.items()))
    held = value(mean)-center if mean is not None else None
    start_sd = value(sd) if sd is not None else num.hypot(num.sqrt(max(0.1, variance)), held or 0)
    B = 1/start_sd
    A = (held or 0)*B
    f, g, H = working_eval(hist, center, A, B, held, sd, num)
    converged = False
    for it in range(maxit):
        if len(g) == 1:
            step = [-g[0]/H[0][0]]
        else:
            det = H[0][0]*H[1][1]-H[0][1]*H[1][0]
            assert det > 0
            step = [(-H[1][1]*g[0]+H[0][1]*g[1])/det,
                    (H[1][0]*g[0]-H[0][0]*g[1])/det]
        slope = sum(a*b for a, b in zip(g, step))
        assert num.isfinite(slope) and slope <= 0
        converged = -slope <= value(1e-10)**2*n
        accepted = False
        for j in range(maxit):
            t = value(0.5)**j
            if t <= 1e-12:
                break
            new_B = B + t*(step[0] if held is not None else step[1] if sd is None else 0)
            new_A = held*new_B if held is not None else A+t*step[0]
            if new_B <= 0:
                continue
            nf, ng, nH = working_eval(hist, center, new_A, new_B, held, sd, num)
            if nf <= f+1e-4*t*slope+1e-14*n:
                A, B, f, g, H = new_A, new_B, nf, ng, nH
                accepted = True
                break
            if converged:
                break
        if converged or not accepted:
            break
    gate = num.sqrt(sum(z*z for z in g)) < 1e-6*n
    return (center+A/B if held is None else mean, 1/B if sd is None else sd), (
        "decrement" if converged else "score gate" if gate else "failure"), it+1


def check_replay():
    smallest = float.fromhex("0x0.0000000000001p-1022")
    B = 1/smallest
    assert math.isinf(B) and math.isnan(0.0*B)
    print("subnormal held sd: inverse overflows and centered location is NaN")

    for exponent in (52, 60, 100):
        delta = math.ldexp(1, -exponent)
        hist = {0: 1}
        h = mp.mpf(delta)
        star = (-h, mp.sqrt((1+2*h)/(2*mp.log1p(1/h))))
        assert abs(exact_eval(hist, *star)[2]) < mp.mpf("1e-65")
        for num in (math, mp):
            y, how, iterations = replay(hist, num=num, mean=-delta)
            err = errors(y, star, {"mean"})[1]
            assert how == "decrement"
            if exponent == 100:
                assert err > 0.01
            ll_star = exact_eval(hist, *star)[0]
            ll_gap = ll_star - exact_eval(hist, *map(mp.mpf, y))[0]
            print("held seam", exponent, num.__name__, how, iterations,
                  "scale", mp.nstr(y[1], 18), "target", mp.nstr(star[1], 18),
                  "relative error", err, "likelihood loss", mp.nstr(ll_gap, 10))

    for hist, held in [({0: 2, 2: 1}, {}), ({0: 2, 2: 1}, {"mean": 1.0}),
                       ({0: 2, 2: 1}, {"sd": 1.0})]:
        y, how, it = replay(hist, num=math, **held)
        star = root(hist, held, y)
        err = errors(y, star, held)
        assert how == "decrement" and max(e for e in err if e is not None) < TOL
        print("default control", held, err, "root", [mp.nstr(v, 18) for v in star])
        rescues = []
        for budget in range(1, it):
            early, why, _ = replay(hist, num=math, maxit=budget, **held)
            early_err = errors(early, star, held)
            if why == "score gate" and max(e for e in early_err if e is not None) > TOL:
                rescues.append((budget, early_err))
        print("out-of-contract capped score accepts", rescues)

    h = -2.0**(-100)
    early, why, _ = replay({0: 1}, num=math, mean=h, maxit=20)
    star = (h, mp.sqrt((1-2*mp.mpf(h))/(2*mp.log1p(-1/mp.mpf(h)))))
    err = errors(early, star, {"mean"})[1]
    assert why == "score gate" and err > 0.01
    print("held seam capped score accept", 20, early[1], err)

    shift = 2**51
    hist = {shift: 2, shift+2: 1}
    y, how, it = replay(hist, num=math)
    assert how == "decrement"
    star = root(hist, {}, y)
    nearest = float(star[0])
    assert nearest == y[0]
    err = errors(y, star, {})
    exact_pair = exact_eval(hist, mp.mpf(y[0]), mp.mpf(y[1]))
    revised_sd = root(hist, {"mean": y[0]}, y)[1]
    relative_refit = errors((y[0], revised_sd), star, {"mean"})[1]
    loss = exact_eval(hist, *star)[0] - exact_pair[0]
    assert max(err) < TOL and loss > mp.mpf("0.01") and relative_refit > 0.005
    print("translated", y, "target location offset", mp.nstr(star[0]-shift, 18),
          "rounding shift", mp.nstr(mp.mpf(nearest)-star[0], 18),
          "errors", err, "likelihood loss", mp.nstr(loss, 12),
          "refit sd", mp.nstr(revised_sd, 18), "refit error", relative_refit)


if __name__ == "__main__":
    mp.mp.dps = 100
    check_actual()
    check_replay()
    print("PASS")
