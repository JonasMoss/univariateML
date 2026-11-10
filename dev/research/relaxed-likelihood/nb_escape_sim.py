"""Escape frequency of the negative binomial fit (size free) to the Poisson face.

The NB MLE with size and probability free exists iff the sample variance
(divisor n) exceeds the sample mean (nbinom dossier); otherwise the fit
escapes to the Poisson limit. Boundary truth (Poisson): frequency -> 1/2.
Interior truth (NB): frequency -> 0. Used in relaxed-likelihood.tex,
section "Asymptotic table". Run: python3 -I nb_escape_sim.py
"""
import numpy as np

rng = np.random.default_rng(1)
cases = [
    ("Poisson(3) truth (boundary)", lambda n, R: rng.poisson(3, (R, n))),
    ("NB truth, size 5, mean 3 (interior)", lambda n, R: rng.negative_binomial(5, 5 / 8, (R, n))),
]
for name, draw in cases:
    print(name)
    for n in [20, 100, 500, 2000]:
        R = 20000
        x = draw(n, R)
        m, v = x.mean(1), x.var(1)
        print("  n=%5d  P(escape: var<=mean) = %.4f" % (n, (v <= m).mean()))
