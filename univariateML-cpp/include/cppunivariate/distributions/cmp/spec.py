"""Independent finite-series quality oracle; omitted mass checked at the golden."""
import sympy as sp
import json
import math
from pathlib import Path

def log_density(symbols, x):
    estimates = json.loads(Path(__file__).with_name('golden.json').read_text())['estimates']
    rate, dispersion = estimates['lambda'], estimates['nu']
    weights = [math.exp(k*math.log(rate)-dispersion*math.lgamma(k+1)) for k in range(129)]
    ratio = rate/129**dispersion
    assert ratio < 1 and weights[128]/(1-ratio)/sum(weights[:128]) < 1e-25
    l, n = symbols['lambda'], symbols['nu']
    z = sum(sp.exp(k*sp.log(l)-n*sp.loggamma(sp.Float(k+1,40))) for k in range(128))
    return x*sp.log(l)-n*sp.loggamma(x+1)-sp.log(z)
