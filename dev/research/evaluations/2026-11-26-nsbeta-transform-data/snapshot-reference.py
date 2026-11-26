"""Independently verify changed joint fits against physical score roots."""
import gzip
import importlib.util
import json
from pathlib import Path
import sys

import mpmath as mp


def read(path):
    stream = gzip.open(path, "rt") if path.suffix == ".gz" else path.open()
    with stream:
        return {key(r): r for r in (json.loads(line) for line in stream if not line.startswith("#"))
                if r["family"] == "nsbeta"}


def key(row):
    return row["source"], row["case"], row["mask"]


def root(case):
    lo, hi = (mp.mpf(case["held"][k]) for k in ("min", "max"))
    y = [(mp.mpf(x)-lo)/(hi-lo) for x in case["data"]]
    n = len(y)
    lx = mp.fsum(map(mp.log, y))/n
    ly = mp.fsum(mp.log1p(-v) for v in y)/n
    mu = mp.fsum(y)/n
    precision = mu*(1-mu)/(mp.fsum((v-mu)**2 for v in y)/n)-1

    def score(u, v):
        a, b = mp.exp(u), mp.exp(v)
        common = mp.digamma(a+b)
        return a*(lx+common-mp.digamma(a)), b*(ly+common-mp.digamma(b))

    u, v = mp.findroot(score, (mp.log(mu*precision), mp.log((1-mu)*precision)),
                      tol=mp.mpf("1e-70"), maxsteps=100)
    a, b = mp.exp(u), mp.exp(v)
    ll = n*((a-1)*lx+(b-1)*ly-mp.log(mp.beta(a,b))-mp.log(hi-lo))
    return a, b, ll


def errors(row, ref, case):
    estimates = row["result"]["estimates"]
    a, b = (mp.mpf(float.fromhex(estimates[k])) for k in ("shape1", "shape2"))
    ll = mp.mpf(float.fromhex(row["result"]["logLik"]))
    lo, hi = (mp.mpf(case["held"][k]) for k in ("min", "max"))
    y = [(mp.mpf(x)-lo)/(hi-lo) for x in case["data"]]
    raw_ll = mp.fsum((a-1)*mp.log(v)+(b-1)*mp.log1p(-v) for v in y)
    raw_ll -= len(y)*(mp.log(mp.beta(a,b))+mp.log(hi-lo))
    return max(abs(a/ref[0]-1), abs(b/ref[1]-1)), abs(ll-raw_ll)/max(1, abs(raw_ll))


if __name__ == "__main__":
    sys.dont_write_bytecode = True
    dev = Path(__file__).resolve().parents[3]
    path = dev/"validation"/"fit-snapshot.py"
    sys.path.insert(0, str(path.parent))
    spec = importlib.util.spec_from_file_location("snapshot", path)
    snap = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(snap)
    archive = dev/"research/evaluations/2026-11-07-fit-audit-data/cases.json.gz"
    with gzip.open(archive, "rt") as stream:
        cases = [r for r in json.load(stream) if r["family"] == "nsbeta"]
    inputs = {key(r): r for r in snap.cases("nsbeta", {"nsbeta": cases})}
    baseline, fresh = map(lambda p: read(Path(p)), sys.argv[1:])
    assert baseline.keys() == fresh.keys()
    print("source,case,mask,shape1_ref,shape2_ref,logLik_ref,old_shape_error,new_shape_error,"
          "old_logLik_error,new_logLik_error")
    for k, row in fresh.items():
        old = baseline[k]
        if old["result"] == row["result"]:
            continue
        with mp.workdps(150):
            low = [mp.nstr(v, 45) for v in root(inputs[k])]
        with mp.workdps(220):
            ref = root(inputs[k])
            assert low == [mp.nstr(v, 45) for v in ref]
            old_shape, old_ll = errors(old, ref, inputs[k])
            shape, ll = errors(row, ref, inputs[k])
            print(*k, *low, *[mp.nstr(v, 8) for v in (old_shape,shape,old_ll,ll)], sep=",")
            assert max(shape, ll) <= mp.mpf("1e-10")
