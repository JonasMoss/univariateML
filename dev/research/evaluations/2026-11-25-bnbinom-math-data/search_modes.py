"""Bounded numerical search for distinct canonical local maxima."""
import itertools
import json
from collections import Counter
from pathlib import Path

import mpmath as mp
import numpy as np

ROOT = Path(__file__).resolve().parents[4]
EVALUATIONS = ROOT / "dev/research/evaluations"
GOLDEN = ROOT / "univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json"
ARCHIVE = EVALUATIONS / "2026-11-08-bnbinom-allfree-data/coverage-samples.jsonl"
STARTS = list(itertools.product((0.2, 1, 6), (0.3, 2, 12), (0.2, 1, 6)))
STARTS = [p for p in STARTS if p[0] <= p[2]] + [(0.15, 2, 20), (2, 2, 20), (2, 12, 20)]


def evaluator(sample):
    n = len(sample)
    tail = np.bincount(sample)[::-1].cumsum()[::-1][1:] / n
    cells = np.arange(len(tail))
    shifts = np.arange(18)

    def psi(x):
        z = x + len(shifts)
        return (np.log(z) - 1/(2*z) - 1/(12*z**2) + 1/(120*z**4)
                - 1/(252*z**6) - np.sum(1/(x+shifts)))

    def psi1(x):
        z = x + len(shifts)
        return (1/z + 1/(2*z**2) + 1/(6*z**3) - 1/(30*z**5)
                + 1/(42*z**7) + np.sum(1/(x+shifts)**2))

    def score_and_hessian(y):
        p = np.exp(y)
        r, a, b = p
        t = a+r+b
        rr, bb, tt = 1/(r+cells), 1/(b+cells), 1/(t+cells)
        ar, ab = psi(a+r), psi(a+b)
        common = psi(t) + tail @ tt
        score = np.array([ar + tail @ rr-common, ar+ab-psi(a)-common,
                          ab + tail @ bb-common])
        u, v = psi1(a+r), psi1(a+b)
        c = psi1(t) - tail @ (tt*tt)
        hessian = np.array([
            [u-tail @ (rr*rr)-c, u-c, -c],
            [u-c, u+v-psi1(a)-c, v-c],
            [-c, v-c, v-tail @ (bb*bb)-c],
        ])
        return p*score, (p[:, None]*hessian*p + np.diag(p*score))

    return score_and_hessian


def propose(evaluate, start):
    y = np.log(start)
    for _ in range(50):
        score, jac = evaluate(y)
        norm = np.linalg.norm(score)
        if norm < 2e-11:
            if np.max(np.linalg.eigvalsh(jac)) < -1e-7:
                p = np.exp(y).tolist()
                p[0], p[2] = sorted((p[0], p[2]))
                return p
            return None
        try:
            step = np.linalg.solve(jac, -score)
        except np.linalg.LinAlgError:
            return None
        step /= max(1, np.linalg.norm(step))
        for power in range(12):
            candidate = y + step * 2**-power
            if np.max(np.abs(candidate)) <= 6 and np.linalg.norm(evaluate(candidate)[0]) < norm:
                y = candidate
                break
        else:
            return None
    return None


def polish(sample, start):
    counts = Counter(sample)

    def ll(r, a, b):
        t = a+r+b
        z = mp.loggamma(a+r)+mp.loggamma(a+b)-mp.loggamma(a)-mp.loggamma(r)-mp.loggamma(b)
        return z+mp.fsum(n*(mp.loggamma(r+x)+mp.loggamma(b+x)-mp.loggamma(t+x)
                            -mp.loggamma(x+1)) for x, n in counts.items())/len(sample)

    def gradient(r, a, b):
        common = mp.fsum(n*mp.digamma(a+r+b+x) for x, n in counts.items())/len(sample)
        size, beta = (mp.fsum(n*mp.digamma(s+x) for x, n in counts.items())/len(sample)
                      - mp.digamma(s) for s in (r, b))
        ar, ab = mp.digamma(a+r), mp.digamma(a+b)
        return ar+size-common, ar+ab-mp.digamma(a)-common, ab+beta-common

    p = tuple(mp.findroot(gradient, start, tol=mp.mpf("1e-60"), maxsteps=40))
    assert max(map(abs, gradient(*p))) < mp.mpf("1e-55")
    hess = mp.matrix([[mp.diff(lambda t: gradient(*(p[:j]+(t,)+p[j+1:]))[i], p[j])
                       for j in range(3)] for i in range(3)])
    assert max(mp.eigsy(hess, eigvals_only=True)) < 0
    return {"coefficients": [mp.nstr(x, 35) for x in p], "average_ll": mp.nstr(ll(*p), 35),
            "numerical_seam": abs(p[0]-p[2]) < mp.mpf("1e-50")}


def main():
    mp.mp.dps = 70
    archived_ids = set(range(0, 216, 7)) | set(range(216, 270, 9))
    samples = {
        "golden": json.loads(GOLDEN.read_text())["data"],
        **{f"spike-{zeros}-{middle}-{extreme}": [0]*zeros + [1]*6 + [3]*middle + [extreme]
           for zeros, middle, extreme in itertools.product((0, 4, 16), (1, 6, 24), (8, 24, 96))},
    }
    for line in ARCHIVE.read_text().splitlines():
        row = json.loads(line)
        if row["id"] in archived_ids and 0 < max(row["x"]) <= 1024:
            samples[f"archive-{row['id']}"] = row["x"]
    output = {}
    for name, sample in samples.items():
        evaluate = evaluator(sample)
        roots = []
        for start in STARTS:
            p = propose(evaluate, start)
            if p is not None and all(np.linalg.norm(np.log(p)-np.log(other)) > 1e-5 for other in roots):
                roots.append(p)
        output[name] = [polish(sample, p) for p in roots]
    assert output["golden"]
    print(json.dumps({
        "dps_for_polish": mp.mp.dps, "starts": STARTS, "sample_count": len(samples),
        "shape_box": ["exp(-6)", "exp(6)"], "root_candidates": output,
    }, indent=2))


if __name__ == "__main__":
    main()
