"""Independent high-precision standardized Prentice gamma tail integrals.

Run through the workstation CPU lock with Python and mpmath installed.
The integration is numerical and is not a rigorous interval enclosure.
"""
import json
import mpmath as mp

mp.mp.dps = 70
result = []
for qstr in ('-0.01', '-0.0001', '-0.000001', '-0.00000001',
             '0', '0.00000001', '0.000001', '0.0001', '0.01'):
    q = mp.mpf(qstr)
    if q:
        a = 1 / q**2
        rem = mp.loggamma(a) - (a-mp.mpf('0.5'))*mp.log(a) + a - mp.log(2*mp.pi)/2
    else:
        rem = mp.mpf(0)
    def density(w):
        z = q*w
        penalty = (mp.expm1(z)-z)/q**2 if q else w*w/2
        return mp.exp(-mp.log(2*mp.pi)/2-rem-penalty)
    for wstr in ('-6', '-1', '0', '1', '6'):
        w = mp.mpf(wstr)
        # Integrate the smaller tail on [-80,80]; truncation is negligible
        # at these Q values but is not enclosed by interval arithmetic.
        if w < 0:
            lower = mp.quad(density, [-80, min(-10, w), w])
            upper = 1-lower
        else:
            upper = mp.quad(density, [w, max(10, w), 80])
            lower = 1-upper
        result.append(dict(Q=qstr, w=wstr, log_lower=mp.nstr(mp.log(lower), 40),
                           log_upper=mp.nstr(mp.log(upper), 40),
                           log_density=mp.nstr(mp.log(density(w)), 40)))
print(json.dumps(result, indent=2))
