#!/usr/bin/env python3
"""Independent actual-hold face comparisons; numerical evidence, not a certificate."""
import json
from pathlib import Path
import mpmath as mp

mp.mp.dps = 80
root = Path(__file__).resolve().parents[4]
rows = []
for family in ("std", "lst"):
    g = json.loads((root / "univariateML-cpp/include/cppunivariate/distributions" /
                    family / "golden.json").read_text())
    x = list(map(mp.mpf, g["data"]))
    location, physical_scale, shape = map(mp.mpf, (g["estimates"][k] for k in
        (("mean", "sd", "nu") if family == "std" else ("mu", "sigma", "df"))))
    scale = physical_scale * mp.sqrt((shape-2)/shape) if family == "std" else physical_scale
    n = len(x)

    def likelihood(m, s, v):
        constant = mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(v*mp.pi)/2-mp.log(s)
        return n*constant-(v+1)*mp.fsum(mp.log1p(((z-m)/s)**2/v) for z in x)/2

    witness = likelihood(location, scale, shape)
    for mask in (1, 2, 3):
        m = location if mask & 1 else mp.fsum(x)/n
        s = physical_scale if mask & 2 else mp.sqrt(mp.fsum((z-m)**2 for z in x)/n)
        normal = -n*(mp.log(2*mp.pi)/2+mp.log(s))-mp.fsum(((z-m)/s)**2 for z in x)/2
        assert witness > normal
        row = dict(family=family, held_mask=mask, finite_golden_witness=str(witness),
                   conditional_normal_loglik=str(normal), normal_gap=str(witness-normal))
        if family == "std" and mask == 1:
            squares = [(z-location)**2 for z in x]
            assert min(squares) > 0
            lo, hi = mp.sqrt(min(squares)), mp.sqrt(mp.mpf("1.5")*mp.fsum(squares)/n)
            for _ in range(320):
                mid = (lo+hi)/2
                score = 3*mp.fsum(q/(2*mid**2+q) for q in squares)-n
                if score > 0:
                    lo = mid
                else:
                    hi = mid
            assert hi-lo < mp.mpf("1e-65")*(lo+hi)/2
            lower = likelihood(location, (lo+hi)/2, mp.mpf(2))
            assert witness > lower
            row.update(conditional_t2_scale=str((lo+hi)/2), conditional_t2_loglik=str(lower),
                       t2_gap=str(witness-lower))
        rows.append(row)
print(json.dumps(dict(precision_dps=80, scope="Numerical actual-hold face comparisons only; no joint root coverage claim", comparisons=rows), indent=2))
