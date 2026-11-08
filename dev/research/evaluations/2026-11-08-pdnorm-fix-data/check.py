"""Independent 100-digit arithmetic for overflowing lattice differences."""
import json
import mpmath as mp
mp.mp.dps = 100
rows = []
for x, mean, sd in [(-1e308, 1e308, 1e308), (1e308, -1e308, 1e308),
                    (-7.976931348623157e307, 1e308, 1e308)]:
    a, m, s = map(mp.mpf, (x, mean, sd))
    z = (a + 1 - m) / s
    # Integrate over a unit cell after removing the scale from the kernel.
    # This avoids subtracting two almost identical normal CDF values.
    cell = mp.quad(lambda u: mp.exp(-((a+u-m)/s)**2/2), [0, 1])
    rows.append(dict(x=x, mean=mean, sd=sd, cdf=str(mp.erfc(-z/mp.sqrt(2))/2),
                     log_density=str(mp.log(cell)-mp.log(2*mp.pi)/2-mp.log(s))))
b = mp.mpf(float.fromhex('0x1.fffffffffffffp-1'))
rows.append(dict(family='dlaplace', x=-1e308, location=1e308, scale=float(b),
                 log_cdf=str(-(mp.mpf(-1e308)-mp.mpf(1e308))*mp.log(b)-mp.log(1+b)),
                 log_density=str(mp.log(1-b)-mp.log(1+b)+abs(mp.mpf(-1e308)-mp.mpf(1e308))*mp.log(b))))
print(json.dumps(rows, indent=2))
