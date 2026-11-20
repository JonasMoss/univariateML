"""Independent tail integrations for the truncated normal archive cases."""
import gzip
import json
from pathlib import Path

import mpmath as mp

mp.mp.dps = 65
root = Path(__file__).resolve().parents[4]
with gzip.open(root / "dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz", "rt") as source:
    cases = json.load(source)

def expectation(t, curvature, statistic):
    def weight(y):
        return mp.exp(-t*y-curvature*y*y)
    return mp.quad(lambda y: weight(y)*statistic(y), [0, 1, mp.inf])/mp.quad(weight, [0, 1, mp.inf])

for index in (7736, 7807, 7819):
    row = cases[index]
    x = list(map(mp.mpf, row["data"]))
    holds = {key: mp.mpf(value) for key, value in row["held"].items()}
    lower = holds.get("b") == mp.inf
    edge = min(x) if lower else max(x)
    orientation = 1 if lower else -1
    s = sum(orientation*(v-edge) for v in x)/len(x)
    if "mean" in holds:
        d = abs(holds["mean"]-edge)
        rho = s/(2*d)
        def statistic(y):
            return y + rho*y*y
        target = sum(statistic(orientation*(v-edge)/s) for v in x)/len(x)
        t = mp.findroot(lambda t: expectation(t, t*rho, statistic)-target, (mp.mpf(".95"), mp.mpf("1.05")))
        print(index, "sd", mp.nstr(mp.sqrt(d*s/t), 45))
    else:
        sd = holds["sd"]
        curvature = (s/sd)**2/2
        t = mp.findroot(lambda t: expectation(t, curvature, lambda y: y)-1, (mp.mpf(".95"), mp.mpf("1.05")))
        print(index, "mean", mp.nstr(edge-orientation*t*sd*sd/s, 45))
