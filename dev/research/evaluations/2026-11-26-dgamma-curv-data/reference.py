#!/usr/bin/env python3
"""Independent dgamma score roots from arbitrary-precision cell integrals."""
from collections import Counter
import json

import mpmath as mp


def solve(data, start, digits):
    with mp.workdps(digits):
        weights = Counter(data)

        def cell(k, shape, rate):
            return mp.gammainc(shape, rate * k, rate * (k + 1))

        def likelihood(shape, rate):
            return sum(w * mp.log(cell(k, shape, rate)) for k, w in weights.items()) - len(data) * mp.loggamma(shape)

        def score(t, u):
            shape, rate = mp.exp(t), mp.exp(u)
            shape_score = mp.diff(lambda a: likelihood(a, rate), shape)
            rate_score = mp.diff(lambda r: likelihood(shape, r), rate)
            return shape * shape_score, rate * rate_score

        t, u = mp.findroot(score, [mp.log(mp.mpf(x)) for x in start])
        assert max(map(abs, score(t, u))) < mp.mpf(10)**(12-digits)
        hessian = mp.matrix([
            [mp.diff(lambda x: score(x, u)[0], t), mp.diff(lambda x: score(t, x)[0], u)],
            [mp.diff(lambda x: score(x, u)[1], t), mp.diff(lambda x: score(t, x)[1], u)],
        ])
        assert hessian[0, 0] < 0 and mp.det(hessian) > 0
        shape, rate = mp.exp(t), mp.exp(u)
        return [mp.nstr(x, 55) for x in (shape, rate, likelihood(shape, rate))]


def main():
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--case", nargs=3, action="append", required=True,
                        metavar=("LABEL", "COUNTS", "START"),
                        help="Comma-separated counts and comma-separated shape and rate start")
    args = parser.parse_args()
    results = {}
    for label, counts, start in args.case:
        data = list(map(int, counts.split(",")))
        first = solve(data, start.split(","), 70)
        second = solve(data, start.split(","), 100)
        assert first == second
        results[label] = dict(data=data, start=start.split(","), shape=first[0], rate=first[1], logLik=first[2])
    print(json.dumps(dict(working_digits=[70, 100], agreement_digits=55, cases=results), indent=2))


if __name__ == "__main__":
    main()
