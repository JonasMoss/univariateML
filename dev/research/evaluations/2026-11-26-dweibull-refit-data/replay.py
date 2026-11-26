"""Compare probe output with independent 90-digit discrete Weibull roots."""

import argparse
import json
import math
import subprocess

from mpmath import mp


def scores(x, alpha, beta):
    a_score = b_score = mp.mpf(0)
    for k in x:
        a, b = (mp.exp(alpha + beta * mp.log(j)) for j in (k, k + 1))
        sa, sb = mp.exp(-a), mp.exp(-b)
        mass = sa - sb
        a_score += (b * sb - a * sa) / mass
        b_score += (b * sb * mp.log(k + 1) - a * sa * mp.log(k)) / mass
    return a_score, b_score


def log_likelihood(x, alpha, beta):
    result = mp.mpf(0)
    for k in x:
        sa, sb = (mp.exp(-mp.exp(alpha + beta * mp.log(j))) for j in (k, k + 1))
        result += mp.log(sa - sb)
    return result


def reference(x, q, beta, start):
    alpha0, beta0 = start
    if math.isnan(q) and math.isnan(beta):
        return tuple(mp.findroot(lambda a, b: scores(x, a, b), start))
    if math.isnan(q):
        return mp.findroot(lambda a: scores(x, a, beta)[0], alpha0), mp.mpf(beta)
    alpha = mp.log(-mp.log(mp.mpf(q)))
    if math.isnan(beta):
        score = lambda b: scores(x, alpha, b)[1]
        lo, hi = mp.mpf(1), beta0 * 2
        assert score(lo) > 0 > score(hi)
        for _ in range(275):
            mid = (lo + hi) / 2
            if score(mid) > 0:
                lo = mid
            else:
                hi = mid
        beta = (lo + hi) / 2
    return alpha, mp.mpf(beta)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("probe")
    parser.add_argument("--expect", choices=("baseline", "fixed"))
    args = parser.parse_args()
    mp.dps = 90
    rows = []
    nan = math.nan
    grid = [(k, k + 2) for k in (5, 6, 7, 8, 9, 10, 12, 15)]
    for x in grid + [(7, 10), (8, 11), (9, 12)]:
        start = (-mp.mpf("0.7") * min(x) * mp.log(max(x)), mp.mpf("0.7") * min(x))
        joint = reference(x, nan, nan, start)
        nearest_q = float(mp.exp(-mp.exp(joint[0])))
        for route, q, beta in [
            ("free", nan, nan),
            ("held-q", min(nearest_q, math.nextafter(1, 0)), nan),
            ("held-beta", nan, float(joint[1])),
            ("both-held", min(nearest_q, math.nextafter(1, 0)), float(joint[1])),
        ]:
            alpha, shape = reference(x, q, beta, joint)
            for score, held in zip(scores(x, alpha, shape), (q, beta)):
                if math.isnan(held):
                    assert abs(score) < mp.mpf("1e-75"), (x, route, score)
            probability = mp.exp(-mp.exp(alpha)) if math.isnan(q) else mp.mpf(q)
            fields = subprocess.check_output([args.probe, *map(str, (*x, q, beta))], text=True).split()
            errors = [mp.nstr(abs(mp.mpf(float(value)) / exact - 1), 12)
                      for value, exact in zip(fields, (probability, shape))]
            public_loglik = loglik_error = None
            if int(fields[3]) == 0:
                public_alpha = mp.log(-mp.log(mp.mpf(float(fields[0]))))
                public_beta = mp.mpf(float(fields[1]))
                public_loglik = log_likelihood(x, public_alpha, public_beta)
                loglik_error = abs(mp.mpf(float(fields[2])) - public_loglik)
            rows.append(dict(x=x, route=route, held_q=None if math.isnan(q) else q.hex(),
                             held_beta=None if math.isnan(beta) else beta.hex(),
                             reference_q=mp.nstr(probability, 55), nearest_q=float(probability).hex(),
                             reference_beta=mp.nstr(shape, 55),
                             fitted_q=fields[0], fitted_beta=fields[1], loglik=fields[2],
                             public_loglik=None if public_loglik is None else mp.nstr(public_loglik, 55),
                             loglik_absolute_error=None if loglik_error is None else mp.nstr(loglik_error, 12),
                             status=int(fields[3]), certified=bool(int(fields[4])),
                             relative_errors=errors))
    if args.expect:
        expected_outside = {(6, 8), (7, 9), (8, 10), (9, 11), (10, 12), (9, 12)}
        outside = set()
        for row in rows:
            x = tuple(row["x"])
            if row["nearest_q"] == float(1).hex():
                assert row["status"] == 8 and not row["certified"]
                continue
            assert row["status"] == 0 and row["certified"], row
            assert float(row["fitted_q"]).hex() == row["nearest_q"], row
            assert float(row["loglik_absolute_error"]) < 1e-12 * max(1, abs(float(row["loglik"]))), row
            if max(map(float, row["relative_errors"])) > 1e-10:
                assert row["route"] == "free", row
                outside.add(x)
        assert outside == (expected_outside if args.expect == "baseline" else set()), outside
    print(json.dumps(rows, indent=2))


if __name__ == "__main__":
    main()
