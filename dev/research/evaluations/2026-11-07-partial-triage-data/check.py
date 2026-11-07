"""Read-only source census and independent checks of the triage derivations.

Run from the repository root through /tmp/umvl-cpu/run. No fitter is invoked.
The inventory enumerates free sets, including unsupported structural sets;
the report supplies their mathematical classification and implementation route.
"""
from pathlib import Path
import hashlib
import itertools
import json
import platform
import subprocess
import sys

import mpmath as mp
import yaml

ROOT = Path(__file__).resolve().parents[4]
OUT = Path(__file__).resolve().parent
DIST = ROOT / "univariateML-cpp/include/cppunivariate/distributions"
mp.mp.dps = 70


def census():
    families = []
    manifest = {}
    for folder in sorted(DIST.iterdir()):
        p = folder / "README.md"
        if not p.is_file():
            continue
        d = yaml.safe_load(p.read_text().split("---", 2)[1])
        if d.get("fit", {}).get("certified") != "partial":
            continue
        names = [p["name"] for p in d["params"]]
        rows = []
        for k in range(1, len(names) + 1):
            for free in itertools.combinations(names, k):
                held = [p for p in names if p not in free]
                structural = [r for r in d.get("holds", [])
                              if set(r["held"]) == set(held)]
                rows.append({"free": list(free), "held": held,
                             "structural": structural})
        families.append({"id": d["id"], "parameters": names,
                         "fit": d["fit"], "subsets": rows})
        for f in sorted(folder.glob("*.hpp")) + [p]:
            manifest[str(f.relative_to(ROOT))] = hashlib.sha256(f.read_bytes()).hexdigest()
    assert len(families) == 29
    assert sum(len(f["subsets"]) for f in families) == 251
    (OUT / "inventory.json").write_text(json.dumps(families, indent=2) + "\n")
    for rel in ["CLAUDE.md", "dev/design/work-budgets.md",
                "dev/design/structured-stop-details.md",
                "dev/roadmap/2026-10-29-parameter-rules.md",
                "univariateML-cpp/include/cppunivariate/distributions/README_SCHEMA.md",
                "univariateML-cpp/include/cppunivariate/distributions/zinb/global_certificate.hpp",
                "univariateML-cpp/include/cppunivariate/special/cmp.hpp"]:
        manifest[rel] = hashlib.sha256((ROOT / rel).read_bytes()).hexdigest()
    (OUT / "source-manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
    return {"partial_families": len(families),
            "nonempty_free_subsets": sum(len(f["subsets"]) for f in families)}


def zip_check():
    # Held-inflation score and the strictly concave log of H.
    worst = mp.mpf(0)
    for z, m, S, pi in itertools.product([1, 10, 99], [1, 5], [5, 20],
                                       [mp.mpf('.01'), mp.mpf('.5'), mp.mpf('.99')]):
        a = pi / (1 - pi)
        score = lambda t: S/t - m - z/(1+a*mp.exp(t))
        for t in [mp.mpf('.05'), mp.mpf('.5'), mp.mpf(2), mp.mpf(10)]:
            p = a*mp.exp(t)/(1+a*mp.exp(t))
            H = lambda u: u*u*a*mp.exp(u)/(1+a*mp.exp(u))**2
            direct = mp.diff(score, t)
            formula = (-S+z*H(t))/(t*t)
            worst = max(worst, abs(direct-formula))
            assert mp.diff(lambda u: mp.log(H(u)), t, 2) < 0
            assert abs(mp.diff(lambda u: mp.log(H(u)), t, 2)
                       - (-2/t**2-2*p*(1-p))) < mp.mpf('1e-60')
        assert score(mp.mpf(S)/(m+z)) >= -mp.mpf('1e-60')
        assert score(mp.mpf(S)/m) <= mp.mpf('1e-60')
    assert worst < mp.mpf('1e-60')
    return {"maximum_derivative_error": str(worst),
            "result": "stationary roots confined to [S/n,S/m]; log H strictly concave"}


def fatigue_check():
    alpha = mp.mpf(3)
    rlo = (7-3*mp.sqrt(5))/2
    rhi = (7+3*mp.sqrt(5))/2
    ll = lambda b: mp.log(1+b)-mp.log(b)/2-mp.log(alpha) - (1/b+b-2)/(2*alpha**2)
    for b in [rlo, mp.mpf(1), rhi]:
        assert abs(mp.diff(ll, b)) < mp.mpf('1e-60')
    assert mp.diff(ll, 1, 2) > 0
    assert ll(rlo) > ll(1) and ll(rhi) > ll(1)
    assert abs(ll(rlo)-ll(rhi)) < mp.mpf('1e-60')
    x = list(map(mp.mpf, ['.3', '1', '2', '7']))
    S, T, n = sum(x), sum(1/v for v in x), len(x)
    errors = []
    for b in list(map(mp.mpf, ['.02', '.5', '3', '20'])):
        score = sum(1/(v+b) for v in x)-n/(2*b)+S/(2*alpha**2*b*b)-T/(2*alpha**2)
        D = mp.fprod(v+b for v in x)
        poly = (2*alpha**2*b*b*sum(mp.fprod(v+b for j,v in enumerate(x) if j!=i)
                                  for i in range(n))
                +(-n*alpha**2*b+S-T*b*b)*D)
        errors.append(abs(score-poly/(2*alpha**2*b*b*D)))
    assert max(errors) < mp.mpf('1e-60')
    return {"sample": [1], "held_alpha": 3,
            "global_beta_maxima": [str(rlo), str(rhi)],
            "loglik_gain_over_beta_one": str(ll(rlo)-ll(1)),
            "maximum_polynomial_identity_error": str(max(errors))}


def genbeta_check():
    y = list(map(mp.mpf, ['.02', '.3', '1', '3']))
    errors = []
    for p,q,a in itertools.product(map(mp.mpf, ['.1', '1', '8']),
                                  map(mp.mpf, ['.1', '.8', '1', '5']),
                                  map(mp.mpf, ['.01', '.5', '8'])):
        ll = lambda t: len(y)*mp.log(t)-p*t*sum(y)+(q-1)*sum(mp.log(-mp.expm1(-t*v)) for v in y)
        curvature = -len(y)/a**2-(q-1)*sum(v*v*mp.exp(-a*v)/(-mp.expm1(-a*v))**2 for v in y)
        errors.append(abs(mp.diff(ll,a,2)-curvature))
        assert curvature < 0
        if q < 1:
            assert curvature <= -len(y)*q/a**2
    assert max(errors) < mp.mpf('1e-60')
    return {"grid_cases": len(errors), "maximum_curvature_identity_error": str(max(errors)),
            "result": "held beta shapes: strictly concave power likelihood for every q>0"}


def truncation_check():
    # Finite retained support gives a valid all-N upper bound even when the
    # original conditioning support is infinite. No Poisson fitter is needed.
    data = [1, 2, 2, 4]
    K, N0 = 6, 10
    correction = lambda N: sum(sum(-mp.log1p(-mp.mpf(j)/N) for j in range(x,K)) for x in data)
    worst = mp.mpf('-inf')
    for N in [N0, 20, 100]:
        for rate in map(mp.mpf, ['.2', '2', '8']):
            c = lambda k: sum(mp.log1p(-mp.mpf(j)/N) for j in range(k))
            fullz = sum(rate**k/mp.factorial(k)*mp.exp(c(k)) for k in range(1,N+1))
            truncatedz = sum(rate**k/mp.factorial(k) for k in range(1,K+1))
            actual = sum(x*mp.log(rate)-mp.loggamma(x+1)+c(x)-mp.log(fullz) for x in data)
            upper = sum(x*mp.log(rate)-mp.loggamma(x+1)-mp.log(truncatedz) for x in data)+correction(N)
            worst = max(worst, actual-upper)
            assert actual <= upper
        assert correction(N+1) < correction(N)
    return {"maximum_actual_minus_upper": str(worst),
            "result": "retained positive support 1..K bounds infinite-window binomial tail"}


def extreme_check():
    errors = []
    for xi in map(mp.mpf, ['-.9', '-.5', '.2', '1']):
        for x, sigma in itertools.product(map(mp.mpf, ['-.1', '.1']),
                                         map(mp.mpf, ['.5', '2'])):
            mu = mp.mpf(0)
            t = 1+xi*(x-mu)/sigma
            ll = lambda u: -(1+1/xi)*mp.log(1+xi*(x-u)/sigma)-(1+xi*(x-u)/sigma)**(-1/xi)
            curvature = (1+xi)/(sigma*sigma*t*t)*(xi-t**(-1/xi))
            errors.append(abs(mp.diff(ll,mu,2)-curvature))
            if xi < 0:
                assert curvature < 0
    assert max(errors) < mp.mpf('1e-60')
    # Fixed-shape profiled Frechet exterior inequalities, n=4,k=1,lambda=2.
    data, lam = list(map(mp.mpf, ['0','1','2','4'])), mp.mpf(2)
    n,k,R = len(data),1,max(data)
    C = n*mp.log(lam*n)-n-n*mp.log(k)-(lam+1)*sum(mp.log(v) for v in data if v>0)
    for gap in map(mp.mpf, ['.0001','.1','1','10','100']):
        d = [v+gap for v in data]
        actual = n*mp.log(lam*n)-n-n*mp.log(sum(v**(-lam) for v in d))-(lam+1)*sum(mp.log(v) for v in d)
        lower_end_upper = C+(lam*(n-k)-k)*mp.log(gap)
        upper_end_upper = n*mp.log(lam)-n+n*lam*mp.log(gap+R)-n*(lam+1)*mp.log(gap)
        assert actual <= lower_end_upper and actual <= upper_end_upper
    return {"maximum_GEV_curvature_identity_error": str(max(errors)),
            "result": "fixed negative GEV shape has strictly concave location; fixed-shape Frechet tail bounds passed"}


def main():
    result = {"source_revision": subprocess.check_output(['git','rev-parse','HEAD'], cwd=ROOT, text=True).strip(),
              "environment": {"python": sys.version, "platform": platform.platform(),
                              "mpmath": mp.__version__, "PyYAML": yaml.__version__, "digits": mp.mp.dps},
              "inventory": census(), "zip": zip_check(), "fatigue": fatigue_check(),
              "genbeta": genbeta_check(), "truncated_binomial": truncation_check(),
              "extreme_value": extreme_check(),
              "seed": "none: deterministic grids", "status": "passed"}
    (OUT/'checks.json').write_text(json.dumps(result, indent=2)+'\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
