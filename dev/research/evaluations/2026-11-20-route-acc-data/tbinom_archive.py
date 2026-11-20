"""Replay the large-count window using exact ratios of represented weights."""
import gzip
import json
from fractions import Fraction
from pathlib import Path

import mpmath as mp

mp.mp.dps = 65
root = Path(__file__).resolve().parents[4]
with gzip.open(root / "dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz", "rt") as source:
    cases = json.load(source)
free, held = cases[7325], cases[7353]
assert free["data"] == held["data"] and not free["held"] and held["held"] == {"a": 0.0}
x = free["data"]
assert cases[7917]["data"] == x
lo, hi = min(x), max(x)
offsets = [v-lo for v in x]
target = mp.mpf(sum(offsets))/len(offsets)

def weights(adjacent_ratio):
    result = [1]
    for k in range(lo, hi):
        result.append(result[-1]*adjacent_ratio(k))
    return result

def mean(weights):
    return sum(j*w for j, w in enumerate(weights))/sum(weights)

def likelihood(weights):
    total = sum(weights)
    return sum(mp.log(mp.mpf((weights[j]/total).numerator)/(weights[j]/total).denominator) for j in offsets)

for size, probability in ((1000011, float.fromhex("0x1.ffff20c43cbfp-1")),
                          (1000035, float.fromhex("0x1.fffc35a371662p-1"))):
    odds = Fraction(probability)/(1-Fraction(probability))
    print("finite witness in free window", size, probability.hex(),
          likelihood(weights(lambda k: Fraction(size-k, k+1)*odds)))

rate = mp.findroot(lambda l: mean(weights(lambda k: l/(k+1)))-target, (mp.mpf(920000), mp.mpf(950000)))
print("Poisson conditional rate", rate)
rate_binary = Fraction(float(rate))
print("Poisson face at represented rate", float(rate).hex(),
      likelihood(weights(lambda k: rate_binary/(k+1))))
q = weights(lambda k: rate/(k+1))
q = [value/sum(q) for value in q]
for j in range(1, hi-lo):
    empirical = mp.mpf(sum(max(j-v, 0) for v in offsets))/len(x)
    fitted = sum(max(j-v, 0)*w for v, w in enumerate(q))
    print("empirical put minus limit put", j, mp.nstr(empirical-fitted, 25))
