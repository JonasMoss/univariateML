"""Independent likelihood scores using the exact binary64 archive inputs."""
import gzip
import json
from pathlib import Path

import mpmath as mp

mp.mp.dps = 100
HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
ARCHIVE = HERE.parent / "2026-11-07-fit-audit-data/cases.json.gz"
IDS = {1625, 1628, 1674, 1675, 1699, 1700, 1702, 1703, 1752, 2191,
       2192, 2194, 2195, 10320, 10321, 10404, 10405, 10736, 10745}


def fit_shapes(logits):
    left = mp.fsum(-mp.log1p(mp.exp(-z)) for z in logits) / len(logits)
    right = mp.fsum(-mp.log1p(mp.exp(z)) for z in logits) / len(logits)
    variance = mp.fsum((z-mp.fsum(logits)/len(logits))**2 for z in logits) / len(logits)
    mu = 1 / (1 + mp.exp(-mp.fsum(logits)/len(logits)))

    def score(lp, lq):
        p, q = mp.exp(lp), mp.exp(lq)
        common = mp.digamma(p+q)
        g, h = common-mp.digamma(p)+left, common-mp.digamma(q)+right
        return g-h, (mu*g+(1-mu)*h)/variance

    lp, lq = mp.findroot(score, (mp.log(1/((1-mu)*variance)),
                                  mp.log(1/(mu*variance))), tol=mp.mpf("1e-75"))
    return mp.exp(lp), mp.exp(lq)


def case_reference(case, baseline):
    held = {k: mp.mpf(v) for k, v in case["held"].items()}
    y = [mp.log(mp.mpf(v)-held.get("min", 0)) for v in case["data"]]
    mean = mp.fsum(y) / len(y)
    n = len(y)

    def likelihood(q, a, p, logb):
        z = [a*(v-logb) for v in y]
        return n*(mp.log(a)-mp.log(mp.beta(p,q)))-mp.fsum(
            v+p*mp.log1p(mp.exp(-u))+q*mp.log1p(mp.exp(u)) for v,u in zip(y,z))

    if "shape1" not in held:
        a = held["shape2"]
        if "scale" not in held:
            # Probe the free-scale profile at fixed centered logits. The
            # envelope derivative is the analytic intercept score.
            rows = []
            for h in [-12,-8,-4,-2,-1,0,1,2,4,8,12]:
                logb = mean-h/a
                z = [a*(v-logb) for v in y]
                p,q = fit_shapes(z)
                slope = mp.fsum(p-(p+q)/(1+mp.exp(-v)) for v in z)
                rows.append(dict(mean_logit=h, logLik=likelihood(q,a,p,logb),
                                 score=slope, shape1=q, shape3=p))
            faces = {}
            for sign in [1,-1]:
                transformed_mean = mp.fsum(mp.exp(sign*a*(v-mean)) for v in y) / n
                gap = mp.log(transformed_mean)
                lp = mp.findroot(lambda lp: (lp-mp.digamma(mp.exp(lp)))/gap-1,
                                 mp.log(.5/gap), tol=mp.mpf("1e-75"))
                shape = mp.exp(lp)
                scale = mp.exp(mean+sign*mp.log(transformed_mean/shape)/a)
                ll = n*(mp.log(a)+shape*(lp-gap-1)-mp.loggamma(shape))-mp.fsum(y)
                faces["trgamma" if sign>0 else "invtrgamma"] = dict(shape=shape, scale=scale, logLik=ll)
            trial = y[0]
            p,q = fit_shapes([a*(v-trial) for v in y])
            witness = dict(shape1=q, shape2=a, shape3=p, scale=mp.exp(trial),
                           logLik=likelihood(q,a,p,trial))
            candidate = baseline[case["index"]]
            qp = mp.mpf(float.fromhex(candidate["shape1"]))
            pp = mp.mpf(float.fromhex(candidate["shape3"]))
            bp = mp.log(mp.mpf(float.fromhex(candidate["scale"])))
            logits = [a*(v-bp) for v in y]
            common = mp.digamma(qp+pp)
            ray_score = pp*(n*(common-mp.digamma(pp))-mp.fsum(mp.log1p(mp.exp(-v)) for v in logits))
            ray_score += qp*(n*(common-mp.digamma(qp))-mp.fsum(mp.log1p(mp.exp(v)) for v in logits))
            return dict(profile=rows, faces=faces, witness=witness,
                        baseline_candidate=dict(logLik=likelihood(qp,a,pp,bp), log_concentration_score=ray_score))
        logb = mp.log(held["scale"])
        p,q = fit_shapes([a*(v-logb) for v in y])
    else:
        q,p = held["shape1"],held["shape3"]

        def intercept(a,c):
            z = [a*(v-mean)-c for v in y]
            r = [p-(p+q)/(1+mp.exp(-u)) for u in z]
            return -mp.fsum(r), n/a+mp.fsum((v-mean)*r_i for v,r_i in zip(y,r))

        if "shape2" in held:
            a = held["shape2"]
            c = mp.findroot(lambda c: intercept(a,c)[0], mp.log(q/p))
        else:
            sd = mp.sqrt(mp.fsum((v-mean)**2 for v in y)/n)
            loga,c = mp.findroot(lambda loga,c: intercept(mp.exp(loga),c),
                                 (-mp.log(sd),mp.log(q/p)))
            a = mp.exp(loga)
        logb = mean+c/a
    return dict(shape1=q, shape2=a, shape3=p, scale=mp.exp(logb), logLik=likelihood(q,a,p,logb))


def main():
    with gzip.open(HERE.parent / "2026-11-15-fit-snapshot-data/native.jsonl.gz", "rt") as lines:
        baseline = {int(row["case"]): row["result"]["candidates"][0]["estimates"]
                    for line in lines for row in [json.loads(line)]
                    if row["family"] == "fpareto" and row["case"] in {"001625","001628"}}
    references = {f"{c['index']:06}": case_reference(c, baseline)
                  for c in json.loads(gzip.decompress(ARCHIVE.read_bytes())) if c["index"] in IDS}
    counts = {0:1000, 1:999, 2:999, 3:999, 4:1000}
    n = sum(counts.values())
    mean = mp.mpf(sum(v*k for v,k in counts.items())) / n
    r = mp.findroot(lambda r: mp.fsum(k*(mp.digamma(r+v)-mp.digamma(r))
                                     for v,k in counts.items()) - n*mp.log1p(mean/r), (3000,3500))
    references["nbinom"] = dict(size=r, prob=r/(r+mean), sample_counts=counts)
    beta = json.loads((ROOT / "univariateML-cpp/include/cppunivariate/distributions/beta/golden.json").read_text())
    x = list(map(mp.mpf,beta["data"]))
    p,q = [mp.mpf(beta["estimates"][key]) for key in ("shape1","shape2")]
    ll = -len(x)*mp.log(mp.beta(p,q))+mp.fsum((p-1)*mp.log(u)+(q-1)*mp.log1p(-u) for u in x)
    references["beta"] = dict(logLik=ll, golden_relative_error=abs(mp.mpf(beta["logLik"])/ll-1))
    print(json.dumps(references, indent=2, default=lambda x: mp.nstr(x,60)))


if __name__ == "__main__":
    main()
