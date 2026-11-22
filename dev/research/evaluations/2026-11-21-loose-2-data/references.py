"""Independent score witnesses for the frozen loose-comparison fixtures."""
import csv
import json
from pathlib import Path
import re
import sys
import subprocess

import mpmath as mp

ROOT = Path(__file__).resolve().parents[4]
DISTS = ROOT / "univariateML-cpp/include/cppunivariate/distributions"


def binary(values):
    return [mp.mpf(float(v)) for v in values]


def golden(family):
    return binary(json.loads((DISTS / family / "golden.json").read_text())["data"])


def bhatt(x, start, hold_sigma=False, symmetric=False):
    sqrt2 = mp.sqrt(2)

    def likelihood(mu, sigma, width):
        return mp.fsum(
            mp.log((mp.erf((v - mu + width) / (sqrt2 * sigma)) -
                    mp.erf((v - mu - width) / (sqrt2 * sigma))) / (4 * width))
            for v in x
        )

    def objective(*free):
        i = iter(free)
        mu = mp.mpf(0) if symmetric else next(i)
        sigma = mp.mpf(1) if hold_sigma else next(i)
        return likelihood(mu, sigma, next(i))

    start = binary(start)
    def score(*p):
        return [mp.diff(objective, p, tuple(int(i == j) for i in range(len(p))))
                for j in range(len(p))]

    return tuple(mp.findroot(score, start, tol=mp.eps * 100, maxsteps=100))


def gb2(x, start, held):
    logs = [mp.log(v) for v in x]
    n = len(x)

    def score(*free):
        it = iter(free)
        a, b, c, scale = [next(it) if v is None else v for v in held]
        t = [v - mp.log(scale) for v in logs]
        z = [b * v for v in t]
        probs = [1 / (1 + mp.exp(-v)) for v in z]
        digamma = mp.digamma(a + c)
        coefficients = (
            n * (digamma - mp.digamma(a)) - mp.fsum(mp.log1p(mp.exp(v)) for v in z),
            n / b + mp.fsum(v * (c - (a + c) * p) for v, p in zip(t, probs)),
            n * (digamma - mp.digamma(c)) + mp.fsum(
                v - mp.log1p(mp.exp(v)) for v in z),
            b / scale * ((a + c) * mp.fsum(probs) - n * c),
        )
        return [v for v, hold in zip(coefficients, held) if hold is None]

    return tuple(mp.findroot(score, binary(start), maxsteps=100))


def references():
    yield "bhatt/golden", bhatt(golden("bhatt"), [-.0038, 1.0579, 1.7336])
    path = (DISTS / "bhatt/test.cpp").relative_to(ROOT)
    source = subprocess.check_output(["git", "show", f"460a0ff2:{path}"], cwd=ROOT, text=True)
    source = source.split('TEST_CASE("Bhatt near-normal width')[1].split(
        'TEST_CASE("Bhatt compensated normal members')[0]
    fixtures = re.findall(r"std::vector<double> x\{([^}]+)\};(.*?)(?=\n  \}|\Z)",
                          source, re.S)
    for i, (data, block) in enumerate(fixtures):
        if 'NAN,0.)' in block or 'NAN,0.);' in block:
            continue
        x = binary(data.split(","))
        symmetric = all(a == -b for a, b in zip(x, reversed(x)))
        hold = "1000,1." in block
        targets = [float(v) for v in re.findall(r"Approx\(([\d.]+)\)", block)]
        start = ([] if symmetric else targets[:1]) + ([] if hold else targets[1:2]) + targets[-1:]
        yield f"bhatt/near/{i}", bhatt(x, start, hold, symmetric)
    yield "gb2/burr", gb2(golden("burr"), [1.3346, 1.8604, 1.197], [None, None, 1, None])
    x = binary([.03,.1,.3,.7,1,2,5,20])
    yield "gb2/mixed/0", gb2(x, [.578,1.794], [2,None,None,1])
    yield "gb2/mixed/1", gb2(x, [3.31,.432], [None,None,3,1])

    x = binary([1e-12, 1.0 - 1e-12])
    def kumar(a):
        z = [mp.log(v) for v in x]
        b = -len(x) / mp.fsum(mp.log(-mp.expm1(a * u)) for u in z)
        return len(x) / a + mp.fsum(u - (b - 1) * u / mp.expm1(-a * u) for u in z)
    yield "kumar/clipped", (mp.findroot(kumar, binary([.0077,.0078])),)

    x = binary([-35.321072334324725,11.459074428000962,-.015351125824354262,
                -.2513099360462538,-297.3459307209368,.5881721220894813])
    xi = mp.mpf(10)
    nu = 3
    first_abs_moment = 2 / mp.pi
    mu = first_abs_moment * (xi - 1 / xi)
    sigma = mp.sqrt((xi - 1 / xi)**2 + 1 - mu**2)
    def sstd(t):
        z = [v * mp.exp(-t) * sigma + mu for v in x]
        return -len(x) * t - (nu + 1) / mp.mpf(2) * mp.fsum(
            mp.log1p((u / (xi if u > 0 else 1 / xi))**2) for u in z)
    yield "sstd/log-scale", (mp.findroot(lambda t: mp.diff(sstd, t), binary([5.89476,5.89477])),)

    x = binary([-3,-1,-.2,.1,.8,2,4])
    def student_cdf(t):
        tail = mp.betainc(mp.mpf("2.5"), mp.mpf(".5"), 0, 5 / (5 + t*t),
                          regularized=True) / 2
        return tail if t < 0 else 1 - tail
    def st(alpha):
        return mp.fsum(mp.log(student_cdf(alpha * v * mp.sqrt(5 / (4 + v*v)))) for v in x)
    yield "st/slant", (mp.findroot(lambda a: mp.diff(st, a), binary([.16,.17])),)

    path = ROOT / "univariateML-cpp/tests/modifiers/fixtures/zinb/golden.json"
    g = json.loads(path.read_text())
    x = g["data"]
    size, pi = binary([g["estimates"]["size"], g["estimates"]["pi"]])
    n, k, total = len(x), sum(v > 0 for v in x), sum(x)
    def zinb(p):
        return ((1 - pi) * p**size * (n * size * (1-p) - total*p)
                - pi * (total*p - k * size * (1-p)))
    p = mp.findroot(zinb, binary([.34,.35]))
    yield "zinb/held-inflation", (p, mp.diff(zinb, p))


def main():
    passes = []
    for precision in (70, 100):
        with mp.workdps(precision):
            passes.append(dict(references()))
    writer = csv.writer(sys.stdout, lineterminator="\n")
    writer.writerow(("fixture", "coordinate", "root_100_digits", "binary64"))
    for key, roots in passes[1].items():
        for i, root in enumerate(roots):
            assert float(root) == float(passes[0][key][i]), (key, i)
            writer.writerow((key, i, mp.nstr(root, 100), repr(float(root))))


if __name__ == "__main__":
    main()
